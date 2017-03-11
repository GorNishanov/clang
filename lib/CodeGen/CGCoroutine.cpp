//===----- CGCoroutine.cpp - Emit LLVM Code for C++ coroutines ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This contains code dealing with C++ code generation of coroutines.
//
//===----------------------------------------------------------------------===//

#include "CGCleanup.h"
#include "CodeGenFunction.h"
#include "llvm/ADT/ScopeExit.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/StmtVisitor.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

// TODO: Improve diagnostic when await_suspend takes no arguments.

using namespace clang;
using namespace CodeGen;

using llvm::Value;
using llvm::BasicBlock;
using llvm::ConstantInt;
using llvm::APInt;

using llvm::Function;
using llvm::DominatorTree;
using llvm::Instruction;
using llvm::SwitchInst;
using llvm::PHINode;
using llvm::Use;

namespace {
enum class AwaitKind { Init, Normal, Yield, Final };
char const *AwaitKindStr[] = {"init", "await", "yield", "final"};
}

namespace clang {
namespace CodeGen {

struct CGCoroData {
  AwaitKind CurrentAwaitKind = AwaitKind::Init;
  llvm::BasicBlock *SuspendBB = nullptr;

  CodeGenFunction::JumpDest CleanupJD;

  // Stores the jump destination just before the final suspend. Coreturn
  // statements jumps to this point after calling return_xxx promise member.
  CodeGenFunction::JumpDest FinalJD;

  unsigned AwaitNum = 0;
  unsigned YieldNum = 0;
  unsigned CoreturnCount = 0;

  // Stores the llvm.coro.id emitted in the function so that we can supply it
  // as the first argument to coro.begin, coro.alloc and coro.free intrinsics.
  // Note: llvm.coro.id returns a token that cannot be directly expressed in a
  // builtin.
  llvm::CallInst *CoroId = nullptr;
  llvm::CallInst *CoroBegin = nullptr;
  llvm::CallInst *LastCoroFree = nullptr;
  // If coro.id came from the builtin, remember the expression to give better
  // diagnostic. If CoroIdExpr is nullptr, the coro.id was created by
  // EmitCoroutineBody.
  CallExpr const *CoroIdExpr = nullptr;
};
}
}

clang::CodeGen::CodeGenFunction::CGCoroInfo::CGCoroInfo() {}
CodeGenFunction::CGCoroInfo::~CGCoroInfo() {}

static void createCoroData(CodeGenFunction &CGF,
                           CodeGenFunction::CGCoroInfo &CurCoro,
                           llvm::CallInst *CoroId,
                           CallExpr const *CoroIdExpr = nullptr) {
  if (CurCoro.Data) {
    if (CurCoro.Data->CoroIdExpr)
      CGF.CGM.Error(CoroIdExpr->getLocStart(),
                    "only one __builtin_coro_id can be used in a function");
    else if (CoroIdExpr)
      CGF.CGM.Error(CoroIdExpr->getLocStart(),
                    "__builtin_coro_id shall not be used in a C++ coroutine");
    else
      llvm_unreachable("EmitCoroutineBodyStatement called twice?");

    return;
  }

  CurCoro.Data = std::unique_ptr<CGCoroData>(new CGCoroData);
  CurCoro.Data->CoroId = CoroId;
  CurCoro.Data->CoroIdExpr = CoroIdExpr;
}

bool CodeGenFunction::isCoroutine() const { return CurCoro.Data != nullptr; }

static SmallString<32> buildSuspendSuffixStr(CGCoroData &Coro, AwaitKind Kind) {
  unsigned No = 0;
  switch (Kind) {
  default:
    break;
  case AwaitKind::Normal:
    No = ++Coro.AwaitNum;
    break;
  case AwaitKind::Yield:
    No = ++Coro.YieldNum;
    break;
  }
  SmallString<32> Suffix(AwaitKindStr[static_cast<int>(Kind)]);
  if (No > 1) {
    Twine(No).toVector(Suffix);
  }
  return Suffix;
}

static Value *emitSuspendExpression(CodeGenFunction &CGF, CGCoroData &Coro,
                                    CoroutineSuspendExpr const &S,
                                    AwaitKind Kind,
                                    ReturnValueSlot ReturnValue) {
  auto &Builder = CGF.Builder;
  const bool IsFinalSuspend = Kind == AwaitKind::Final;
  auto Suffix = buildSuspendSuffixStr(Coro, Kind);

  auto *E = S.getCommonExpr();
  if (auto *UO = dyn_cast<UnaryOperator>(E))
    if (UO->getOpcode() == UO_Coawait)
      E = UO->getSubExpr();

  auto Binder = CodeGenFunction::OpaqueValueMappingData::bind(
      CGF, S.getOpaqueValue(), E);
  auto UnbindOnExit = llvm::make_scope_exit([&] { Binder.unbind(CGF); });

  BasicBlock *ReadyBlock = CGF.createBasicBlock(Suffix + Twine(".ready"));
  BasicBlock *SuspendBlock = CGF.createBasicBlock(Suffix + Twine(".suspend"));
  BasicBlock *CleanupBlock = CGF.createBasicBlock(Suffix + Twine(".cleanup"));

  CGF.EmitBranchOnBoolExpr(S.getReadyExpr(), ReadyBlock, SuspendBlock, 0);
  CGF.EmitBlock(SuspendBlock);

  llvm::Function *CoroSave = CGF.CGM.getIntrinsic(llvm::Intrinsic::coro_save);
  auto *NullPtr = llvm::ConstantPointerNull::get(CGF.CGM.Int8PtrTy);
  auto *SaveCall = Builder.CreateCall(CoroSave, {NullPtr});

  auto *SuspendRet = CGF.EmitScalarExpr(S.getSuspendExpr());
  if (SuspendRet != nullptr) {
    // FIXME: Add proper error if the result of the expression is not bool.
    if (!SuspendRet->getType()->isIntegerTy(1)) {
      CGF.ErrorUnsupported(S.getSuspendExpr(), "non void and non bool await_suspend");
      return nullptr;
    }
    BasicBlock *RealSuspendBlock =
        CGF.createBasicBlock(Suffix + Twine(".suspend.bool"));
    CGF.Builder.CreateCondBr(SuspendRet, RealSuspendBlock, ReadyBlock);
    SuspendBlock = RealSuspendBlock;
    CGF.EmitBlock(RealSuspendBlock);
  }

  llvm::Function *CoroSuspend =
      CGF.CGM.getIntrinsic(llvm::Intrinsic::coro_suspend);
  auto *SuspendResult = Builder.CreateCall(
      CoroSuspend, {SaveCall, Builder.getInt1(IsFinalSuspend)});
  auto *Switch = Builder.CreateSwitch(SuspendResult, Coro.SuspendBB, 2);
  Switch->addCase(Builder.getInt8(0), ReadyBlock);
  Switch->addCase(Builder.getInt8(1), CleanupBlock);

  CGF.EmitBlock(CleanupBlock);

  CGF.EmitBranchThroughCleanup(Coro.CleanupJD);

  // Emit await_resume expression.
  CGF.EmitBlock(ReadyBlock);
  QualType Type = S.getResumeExpr()->getType();
  switch (CGF.getEvaluationKind(Type)) {
  case TEK_Scalar:
    return CGF.EmitScalarExpr(S.getResumeExpr());
  case TEK_Aggregate:
    CGF.EmitAggExpr(S.getResumeExpr(),
                    AggValueSlot::forAddr(ReturnValue.getValue(), Qualifiers(),
                                          AggValueSlot::IsDestructed,
                                          AggValueSlot::DoesNotNeedGCBarriers,
                                          AggValueSlot::IsNotAliased));
    break;
  case TEK_Complex:
    CGF.CGM.ErrorUnsupported(S.getResumeExpr(), "_Complex await expression");
    break;
  }
  return nullptr;
}

void CodeGenFunction::EmitCoreturnStmt(CoreturnStmt const &S) {
  ++CurCoro.Data->CoreturnCount;
  EmitStmt(S.getPromiseCall());
  EmitBranchThroughCleanup(CurCoro.Data->FinalJD);
}

llvm::Value *CodeGenFunction::EmitCoawaitExpr(const CoawaitExpr &E,
                                              ReturnValueSlot ReturnValue) {
  return emitSuspendExpression(*this, *CurCoro.Data, E,
                               CurCoro.Data->CurrentAwaitKind, ReturnValue);
}
llvm::Value *CodeGenFunction::EmitCoyieldExpr(const CoyieldExpr &E,
                                              ReturnValueSlot ReturnValue) {
  return emitSuspendExpression(*this, *CurCoro.Data, E, AwaitKind::Yield,
                               ReturnValue);
}

// Hunts for the parameter reference in the parameter copy/move declaration.
namespace {
struct GetParamRef : public StmtVisitor<GetParamRef> {
public:
  DeclRefExpr *Expr = nullptr;
  GetParamRef() {}
  void VisitDeclRefExpr(DeclRefExpr *E) {
    assert(Expr == nullptr && "multilple declref in param move");
    Expr = E;
  }
  void VisitStmt(Stmt *S) {
    for (auto *C : S->children()) {
      if (C)
        Visit(C);
    }
  }
};
}

// This class replaces references to parameters to their copies by changing
// the addresses in CGF.LocalDeclMap and restoring back the original values in
// its destructor.

namespace {
  struct ParamReferenceReplacerRAII {
    CodeGenFunction::DeclMapTy SavedLocals;
    CodeGenFunction::DeclMapTy& LocalDeclMap;

    ParamReferenceReplacerRAII(CodeGenFunction::DeclMapTy &LocalDeclMap)
        : LocalDeclMap(LocalDeclMap) {}

    void addCopy(DeclStmt const *PM) {
      // Figure out what param it refers to.

      assert(PM->isSingleDecl());
      VarDecl const*VD = static_cast<VarDecl const*>(PM->getSingleDecl());
      Expr const *InitExpr = VD->getInit();
      GetParamRef Visitor;
      Visitor.Visit(const_cast<Expr*>(InitExpr));
      assert(Visitor.Expr);
      auto *DREOrig = cast<DeclRefExpr>(Visitor.Expr);
      auto *PD = DREOrig->getDecl();

      auto it = LocalDeclMap.find(PD);
      assert(it != LocalDeclMap.end() && "parameter is not found");
      SavedLocals.insert({ PD, it->second });

      auto copyIt = LocalDeclMap.find(VD);
      assert(copyIt != LocalDeclMap.end() && "parameter copy is not found");
      it->second = copyIt->getSecond();
    }

    ~ParamReferenceReplacerRAII() {
      for (auto&& SavedLocal : SavedLocals) {
        LocalDeclMap.insert({SavedLocal.first, SavedLocal.second});
      }
    }
  };
}

#if 0 // TODO: Finish me to support elision of the parameter copies.
static void EmitCoroParam(CodeGenFunction &CGF, DeclStmt *PM) {
  assert(PM->isSingleDecl());
  VarDecl *VD = static_cast<VarDecl *>(PM->getSingleDecl());
  Expr *InitExpr = VD->getInit();
  GetParamRef Visitor;
  Visitor.Visit(InitExpr); // InitExpr);
                           //  Visitor.TraverseStmtExpr(InitExpr);// InitExpr);
  assert(Visitor.Expr);
  auto DREOrig = cast<DeclRefExpr>(Visitor.Expr);

  DeclRefExpr DRE(VD, /* RefersToEnclosingVariableOrCapture= */ false,
                  VD->getType(), VK_LValue, SourceLocation{});
  auto Orig = CGF.Builder.CreateBitCast(CGF.EmitLValue(DREOrig).getAddress(),
                                        CGF.VoidPtrTy);
  auto Copy = CGF.Builder.CreateBitCast(CGF.EmitLValue(&DRE).getAddress(),
                                        CGF.VoidPtrTy);
  SmallVector<Value *, 2> args{Orig.getPointer(), Copy.getPointer()};

  // TODO:
  //  Surround CTOR and DTOR for parameters with
  //     if (coro.param(alloca.copy, alloca.original)) CTOR(...);
  //     if (coro.param(alloca.copy, alloca.original)) DTOR(...);
}
#endif

static SmallVector<llvm::OperandBundleDef, 1>
getBundlesForCoroEnd(CodeGenFunction &CGF) {
  SmallVector<llvm::OperandBundleDef, 1> BundleList;

  if (Instruction *EHPad = CGF.CurrentFuncletPad)
    BundleList.emplace_back("funclet", EHPad);

  return BundleList;
}

namespace {
// We will insert coro.end to cut any of the destructors for objects that
// do not need to be destroyed onces the coroutine is resumed.
struct CallCoroEnd final : public EHScopeStack::Cleanup {
  void Emit(CodeGenFunction &CGF, Flags flags) override {
    auto &CGM = CGF.CGM;
    auto *NullPtr = llvm::ConstantPointerNull::get(CGF.Int8PtrTy);
    llvm::Function *CoroEndFn = CGM.getIntrinsic(llvm::Intrinsic::coro_end);
    auto Bundles = getBundlesForCoroEnd(CGF);
    auto *CoroEnd = CGF.Builder.CreateCall(
        CoroEndFn, {NullPtr, CGF.Builder.getTrue()}, Bundles);
    if (Bundles.empty()) {
      auto *ResumeBB = CGF.getEHResumeBlock(/*cleanup=*/true);
      auto *CleanupContBB = CGF.createBasicBlock("cleanup.cont");
      CGF.Builder.CreateCondBr(CoroEnd, ResumeBB, CleanupContBB);
      CGF.EmitBlock(CleanupContBB);
    }
  }
};
}

namespace {
// Make sure to call coro.delete on scope exit.
struct CallCoroDelete final : public EHScopeStack::Cleanup {
  llvm::Value *CoroId;
  llvm::Value *CoroBegin;
  Stmt *Deallocate;

  // Emit "if (coro.free(CoroId, CoroBegin)) Deallocate;"
  void Emit(CodeGenFunction &CGF, Flags flags) override {
    // Remember the current point, as we are going to emit deallocation code
    // first to get to coro.free instruction that is an argument to a delete
    // call.
    BasicBlock *SaveInsertBlock = CGF.Builder.GetInsertBlock();

    auto *AfterFreeBB = CGF.createBasicBlock("after.coro.free");
    auto *FreeBB = CGF.createBasicBlock("coro.free");

    CGF.EmitBlock(FreeBB);
    CGF.EmitStmt(Deallocate);
    CGF.EmitBlock(AfterFreeBB);

    auto *CoroFree = CGF.CurCoro.Data->LastCoroFree;
    if (!CoroFree)
      return;

    auto *InsertPt = SaveInsertBlock->getTerminator();
    CoroFree->moveBefore(InsertPt);
    CGF.Builder.SetInsertPoint(InsertPt);

    auto *NullPtr = llvm::ConstantPointerNull::get(CGF.Int8PtrTy);
    auto *Cond = CGF.Builder.CreateICmpNE(CoroFree, NullPtr);
    CGF.Builder.CreateCondBr(Cond, FreeBB, AfterFreeBB);

    InsertPt->eraseFromParent();
    CGF.Builder.SetInsertPoint(AfterFreeBB);
  }
  CallCoroDelete(llvm::Value *CoroId, llvm::Value *CoroBegin, Stmt *S)
      : CoroId(CoroId), CoroBegin(CoroBegin), Deallocate(S) {}
};
}

namespace {
struct GetReturnObjectManager {
  CodeGenFunction &CGF;
  CGBuilderTy &Builder;
  const CoroutineBodyStmt &S;

  Address GroActiveFlag;
  CodeGenFunction::AutoVarEmission GroEmission;

  GetReturnObjectManager(CodeGenFunction &CGF, const CoroutineBodyStmt &S)
      : CGF(CGF), Builder(CGF.Builder), S(S), GroActiveFlag(Address::invalid()),
        GroEmission(CodeGenFunction::AutoVarEmission::invalid()) {}

  void EmitGroAlloca() {
    auto *GroDeclStmt = dyn_cast<DeclStmt>(S.getResultDecl());
    if (!GroDeclStmt) {
      // If get_return_object returns void, no need to do an alloca.
      return;
    }

    auto *GroVarDecl = cast<VarDecl>(GroDeclStmt->getSingleDecl());

    // Set GRO flag that it is not initialized yet
    GroActiveFlag =
      CGF.CreateTempAlloca(Builder.getInt1Ty(), CharUnits::One(), "gro.active");
    Builder.CreateStore(Builder.getFalse(), GroActiveFlag);

    GroEmission = CGF.EmitAutoVarAlloca(*GroVarDecl);
    CGF.EmitAutoVarCleanups(GroEmission);

    if (auto *Cleanup = dyn_cast_or_null<EHCleanupScope>(&*CGF.EHStack.begin())) {
      assert(!Cleanup->hasActiveFlag() && "cleanup already has active flag?");
      Cleanup->setActiveFlag(GroActiveFlag);
      Cleanup->setTestFlagInEHCleanup();
      Cleanup->setTestFlagInNormalCleanup();
    }
  }

  void EmitGroInit() {
    if (!GroActiveFlag.isValid()) {
      // No Gro variable was allocated. Simply emit the call to
      // get_return_object.
      CGF.EmitStmt(S.getResultDecl());
      return;
    }

    CGF.EmitAutoVarInit(GroEmission);
    Builder.CreateStore(Builder.getTrue(), GroActiveFlag);
  }
};
}

void CodeGenFunction::EmitCoroutineBody(const CoroutineBodyStmt &S) {
  auto *NullPtr = llvm::ConstantPointerNull::get(Builder.getInt8PtrTy());
  auto &TI = CGM.getContext().getTargetInfo();
  unsigned NewAlign = TI.getNewAlign() / TI.getCharWidth();

  auto *CoroId = Builder.CreateCall(
      CGM.getIntrinsic(llvm::Intrinsic::coro_id),
      {Builder.getInt32(NewAlign), NullPtr, NullPtr, NullPtr});

  auto *CoroAlloc = Builder.CreateCall(
      CGM.getIntrinsic(llvm::Intrinsic::coro_alloc), {CoroId});

  auto *EntryBB = Builder.GetInsertBlock();
  auto *AllocBB = createBasicBlock("coro.alloc");
  auto *FinalBB = createBasicBlock("coro.final");
  auto *InitBB = createBasicBlock("coro.init");
  auto *RetBB = createBasicBlock("coro.ret");

  createCoroData(*this, CurCoro, CoroId);
  CurCoro.Data->SuspendBB = RetBB;

  Builder.CreateCondBr(CoroAlloc, AllocBB, InitBB);

  EmitBlock(AllocBB);

  auto *AllocateCall = EmitScalarExpr(S.getAllocate());
  auto *AllocOrInvokeContBB = Builder.GetInsertBlock();
  Builder.CreateBr(InitBB);

  EmitBlock(InitBB);
  auto *Phi = Builder.CreatePHI(VoidPtrTy, 2);
  Phi->addIncoming(NullPtr, EntryBB);
  Phi->addIncoming(AllocateCall, AllocOrInvokeContBB);

  auto *CoroBegin = Builder.CreateCall(
      CGM.getIntrinsic(llvm::Intrinsic::coro_begin), {CoroId, Phi});
  CurCoro.Data->CoroBegin = CoroBegin;

  GetReturnObjectManager GroManager(*this, S);
  GroManager.EmitGroAlloca();

  CurCoro.Data->CleanupJD = getJumpDestInCurrentScope(RetBB);

  {
    ParamReferenceReplacerRAII ParamReplacer(LocalDeclMap);
    CodeGenFunction::RunCleanupsScope ResumeScope(*this);

    EHStack.pushCleanup<CallCoroDelete>(NormalAndEHCleanup, CoroId, CoroBegin,
                                        S.getDeallocate());

    EmitStmt(S.getPromiseDeclStmt());

    Address PromiseAddr = GetAddrOfLocalVar(S.getPromiseDecl());
    auto *PromiseAddrVoidPtr =
        new llvm::BitCastInst(PromiseAddr.getPointer(), VoidPtrTy, "", CoroId);
    // Update CoroId to refer to the promise. We could not do it earlier because
    // promise local variable was not emitted yet.
    CoroId->setArgOperand(1, PromiseAddrVoidPtr);

    // Now we have the promise, initialize the GRO
    GroManager.EmitGroInit();

    for (auto *PM : S.getParamMoves()) {
      EmitStmt(PM);
      ParamReplacer.addCopy(cast<DeclStmt>(PM));
      // TODO: if(CoroParam(...)) need to surround ctor and dtor
      // for the copy, so that llvm can elide it if the copy is
      // not needed.
    }

    EHStack.pushCleanup<CallCoroEnd>(EHCleanup);

    CurCoro.Data->CurrentAwaitKind = AwaitKind::Init;
    EmitStmt(S.getInitSuspendStmt());
    CurCoro.Data->FinalJD = getJumpDestInCurrentScope(FinalBB);

    CurCoro.Data->CurrentAwaitKind = AwaitKind::Normal;
    auto* Body = S.getBodyInTryCatch();
    EmitStmt(Body ? Body : S.getBody());

    // See if we need to generate final suspend.
    const bool CanFallthrough = Builder.GetInsertBlock();
    const bool HasCoreturns = CurCoro.Data->CoreturnCount > 0;
    if (auto *OnFallthrough = S.getFallthroughHandler())
      if (CanFallthrough && !S.getBodyInTryCatch())
        EmitStmt(OnFallthrough);
    if (CanFallthrough || HasCoreturns) {
      EmitBlock(FinalBB);
      CurCoro.Data->CurrentAwaitKind = AwaitKind::Final;
      EmitStmt(S.getFinalSuspendStmt());
    }
  }

  EmitBlock(RetBB);
  llvm::Function *CoroEnd = CGM.getIntrinsic(llvm::Intrinsic::coro_end);
  Builder.CreateCall(CoroEnd, {NullPtr, Builder.getInt1(0)});

  if (auto RetStmt = S.getReturnStmt())
    EmitStmt(RetStmt);
}

// Emit coroutine intrinsic and patch up arguments of the token type.
RValue CodeGenFunction::EmitCoroutineIntrinsic(const CallExpr *E,
                                               unsigned int IID) {
  SmallVector<llvm::Value *, 8> Args;
  switch (IID) {
  default:
    break;
  // The coro.frame builtin is replaced with a SSA value of the coro.begin
  // intrinsic.
  case llvm::Intrinsic::coro_frame: {
    if (CurCoro.Data && CurCoro.Data->CoroBegin) {
      return RValue::get(CurCoro.Data->CoroBegin);
    }
    CGM.Error(E->getLocStart(), "this builtin expect that __builtin_coro_begin "
      "has been used earlier in this function");
    auto NullPtr = llvm::ConstantPointerNull::get(Builder.getInt8PtrTy());
    return RValue::get(NullPtr);
  }
  // The following three intrinsics take a token parameter referring to a token
  // returned by earlier call to @llvm.coro.id. Since we cannot represent it in
  // builtins, we patch it up here.
  case llvm::Intrinsic::coro_alloc:
  case llvm::Intrinsic::coro_begin:
  case llvm::Intrinsic::coro_free: {
    if (CurCoro.Data && CurCoro.Data->CoroId) {
      Args.push_back(CurCoro.Data->CoroId);
      break;
    }
    CGM.Error(E->getLocStart(), "this builtin expect that __builtin_coro_id has"
                                " been used earlier in this function");
    // Fallthrough to the next case to add TokenNone as the first argument.
  }
  // @llvm.coro.suspend takes a token parameter. Add token 'none' as the first
  // argument.
  case llvm::Intrinsic::coro_suspend:
    Args.push_back(llvm::ConstantTokenNone::get(getLLVMContext()));
    break;
  }
  for (auto &Arg : E->arguments())
    Args.push_back(EmitScalarExpr(Arg));

  llvm::Value *F = CGM.getIntrinsic(IID);
  llvm::CallInst *Call = Builder.CreateCall(F, Args);

  // If we see @llvm.coro.id remember it in the CoroData. We will update
  // coro.alloc, coro.begin and coro.free intrinsics to refer to it.
  if (IID == llvm::Intrinsic::coro_id) {
    createCoroData(*this, CurCoro, Call, E);
  }
  else if (IID == llvm::Intrinsic::coro_begin) {
    if (CurCoro.Data)
      CurCoro.Data->CoroBegin = Call;
  }
  else if (IID == llvm::Intrinsic::coro_free) {
    // Remember the last coro_free as we need it to build the conditional
    // deletion of the coroutine frame.
    if (CurCoro.Data)
      CurCoro.Data->LastCoroFree = Call;
  }
  return RValue::get(Call);
}
