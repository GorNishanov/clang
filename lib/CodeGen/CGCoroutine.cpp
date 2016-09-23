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

#include "CodeGenFunction.h"
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
  LabelDecl *DeleteLabel = nullptr;
  llvm::BasicBlock *SuspendBB = nullptr;

  unsigned AwaitNum = 0;
  unsigned YieldNum = 0;
};
}
}

clang::CodeGen::CodeGenFunction::CGCoroInfo::CGCoroInfo() {}
CodeGenFunction::CGCoroInfo::~CGCoroInfo() {}

bool CodeGenFunction::isCoroutine() const { return CurCoro.Data != nullptr; }

namespace {
struct OpaqueValueMappings {
  LValue common;

  CodeGenFunction::OpaqueValueMapping o1;
  CodeGenFunction::OpaqueValueMapping o2;
  CodeGenFunction::OpaqueValueMapping o3;

  static OpaqueValueExpr *opaque(Expr *E) {
    return cast<OpaqueValueExpr>(
        cast<CXXMemberCallExpr>(E)->getImplicitObjectArgument());
  }

  OpaqueValueMappings(CodeGenFunction &CGF, CoroutineSuspendExpr const &S)
      : common(CGF.EmitMaterializeTemporaryExpr(
            cast<MaterializeTemporaryExpr>(S.getCommonExpr()))),
        o1(CGF, opaque(S.getReadyExpr()), common),
        o2(CGF, opaque(S.getSuspendExpr()), common),
        o3(CGF, opaque(S.getResumeExpr()), common) {}
};
}

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
                                    AwaitKind Kind) {
  auto &Builder = CGF.Builder;
  const bool IsFinalSuspend = Kind == AwaitKind::Final;
  auto Suffix = buildSuspendSuffixStr(Coro, Kind);

  OpaqueValueMappings ovm(CGF, S);

  BasicBlock *ReadyBlock = CGF.createBasicBlock(Suffix + Twine(".ready"));
  BasicBlock *SuspendBlock = CGF.createBasicBlock(Suffix + Twine(".suspend"));
  BasicBlock *CleanupBlock = CGF.createBasicBlock(Suffix + Twine(".cleanup"));

  CodeGenFunction::RunCleanupsScope AwaitExprScope(CGF);

  CGF.EmitBranchOnBoolExpr(S.getReadyExpr(), ReadyBlock, SuspendBlock, 0);
  CGF.EmitBlock(SuspendBlock);

  llvm::Function *CoroSave = CGF.CGM.getIntrinsic(llvm::Intrinsic::coro_save);
  auto NullPtr = llvm::ConstantPointerNull::get(CGF.CGM.Int8PtrTy);
  auto SaveCall = Builder.CreateCall(CoroSave, {NullPtr});

  // FIXME: Handle bool returning suspendExpr.
  CGF.EmitScalarExpr(S.getSuspendExpr());

  llvm::Function *CoroSuspend =
      CGF.CGM.getIntrinsic(llvm::Intrinsic::coro_suspend);
  SmallVector<Value *, 1> args2{SaveCall, Builder.getInt1(IsFinalSuspend)};
  auto SuspendResult = Builder.CreateCall(CoroSuspend, args2);
  auto Switch = Builder.CreateSwitch(SuspendResult, Coro.SuspendBB, 2);
  Switch->addCase(Builder.getInt8(0), ReadyBlock);
  Switch->addCase(Builder.getInt8(1), CleanupBlock);

  CGF.EmitBlock(CleanupBlock);

  // FIXME: This does not work if co_await exp result is used
  //   see clang/test/Coroutines/brokenIR.cpp.
  auto jumpDest = CGF.getJumpDestForLabel(Coro.DeleteLabel);
  CGF.EmitBranchThroughCleanup(jumpDest);

  CGF.EmitBlock(ReadyBlock);
  return CGF.EmitScalarExpr(S.getResumeExpr());
}

// If await expression result is used we end up with broken IR with
// definition of result of the await expression not dominating its uses.
// It results from the way how cleanup blocks are threaded. Here is an
// example of the broken IR we are fixing up.
//
//    %23 = call i8 @llvm.coro.suspend(token % 21, i1 false)
//    switch i8 %23, label %coro.ret [ i8 0, label %await.ready
//                                     i8 1, label %await.cleanup]
// await.cleanup:
//    store i32 2, i32* %cleanup.dest.slot
//    br label %cleanup10
// await.ready:
//    %await.resume = call await_resume(%struct.A* %ref.tmp6)
//    store i32 0, i32* %cleanup.dest.slot
//    br label %common.cleanup
// common.cleanup:
//    ...
//    %cleanup.dest = load i32, i32* %cleanup.dest.slot
//    switch i32 %cleanup.dest, label %unreach[i32 0, label %fallthru
//                                             i32 2, label %more.cleanup]
// fallthru:
//    use of %await.resume
//
// We are fixing this by inserting a PHINode in the cleanup block with
// all values undefined but the one coming out of await.ready edge.

static void fixBrokenUse(Use &U) {
  // Verify that we have the pattern above.
  Instruction &I = *cast<Instruction>(U.getUser());
  Instruction &D = *cast<Instruction>(U.get());

  BasicBlock *DefBlock = D.getParent();
  BasicBlock *PostDefBlock = DefBlock->getSingleSuccessor();
  if (!PostDefBlock)
    return;
  if (PostDefBlock->getSinglePredecessor() != nullptr)
    return;

  // Do a few more sanity checks.
  BasicBlock *FallthruBB = I.getParent();
  BasicBlock *CommonCleanupBB = FallthruBB->getSinglePredecessor();
  if (!CommonCleanupBB)
    return;
  SwitchInst *SI = dyn_cast<SwitchInst>(CommonCleanupBB->getTerminator());
  if (!SI)
    return;

  // Okay, looks like it is cleanup related break that we can fix.
  auto Phi = PHINode::Create(D.getType(), 2, "", &PostDefBlock->front());
  auto Undef = llvm::UndefValue::get(D.getType());
  for (BasicBlock *Pred : predecessors(PostDefBlock))
    Phi->addIncoming(Pred == DefBlock ? (Value *)&D : Undef, Pred);

  D.replaceUsesOutsideBlock(Phi, PostDefBlock);
}

static void runHorribleHackToFixupCleanupBlocks(Function &F) {
  DominatorTree DT(F);
  SmallVector<Use *, 8> BrokenUses;

  for (Instruction &I : instructions(F)) {
    for (Use &U : I.uses()) {
      if (isa<llvm::PHINode>(U.getUser()))
        break;
      if (!DT.dominates(&I, U)) {
        BrokenUses.push_back(&U);
        break; // Go to the next instruction.
      }
    }
  }

  for (Use *U : BrokenUses)
    fixBrokenUse(*U);
}

void CodeGenFunction::EmitCoreturnStmt(CoreturnStmt const &S) {
  EmitStmt(S.getPromiseCall());
}

Value *CodeGenFunction::EmitCoawaitExpr(CoawaitExpr const &S) {
  return emitSuspendExpression(*this, *CurCoro.Data, S,
                               CurCoro.Data->CurrentAwaitKind);
}
Value *CodeGenFunction::EmitCoyieldExpr(CoyieldExpr const &S) {
  return emitSuspendExpression(*this, *CurCoro.Data, S, AwaitKind::Yield);
}

namespace {
struct GetParamRef : public StmtVisitor<GetParamRef> {
public:
  DeclRefExpr *Expr = nullptr;
  GetParamRef() {}
  void VisitDeclRefExpr(DeclRefExpr *E) {
    assert(Expr == nullptr && "multilple declref in param move");
    Expr = E;
  }
};
}

#if 0 // UNUSED for now (to squash the warning)
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
  //  declare i1 @llvm.coro.param(i8* copy, i8* original)
}
#endif

void CodeGenFunction::EmitCoroutineBody(const CoroutineBodyStmt &S) {
  auto *NullPtr = llvm::ConstantPointerNull::get(Builder.getInt8PtrTy());

  // FIXME: Instead of 0, pass an equivalent of alignas(maxalign_t).
  auto *CoroId =
      Builder.CreateCall(CGM.getIntrinsic(llvm::Intrinsic::coro_id),
                         {Builder.getInt32(0), NullPtr, NullPtr, NullPtr});
  auto *CoroAlloc = Builder.CreateCall(
      CGM.getIntrinsic(llvm::Intrinsic::coro_alloc), {CoroId});

  auto *EntryBB = Builder.GetInsertBlock();
  auto *AllocBB = createBasicBlock("coro.alloc");
  auto *InitBB = createBasicBlock("coro.init");
  auto *RetBB = createBasicBlock("coro.ret");

  CurCoro.Data = std::unique_ptr<CGCoroData>(new CGCoroData);
  CurCoro.Data->SuspendBB = RetBB;
  CurCoro.Data->DeleteLabel = S.getDeallocate()->getDecl();

  Builder.CreateCondBr(CoroAlloc, AllocBB, InitBB);

  EmitBlock(AllocBB);

  auto *AllocateCall = EmitScalarExpr(S.getAllocate());
  auto *AllocOrInvokeContBB = Builder.GetInsertBlock();
  Builder.CreateBr(InitBB);

  EmitBlock(InitBB);
  auto *Phi = Builder.CreatePHI(VoidPtrTy, 2);
  Phi->addIncoming(NullPtr, EntryBB);
  Phi->addIncoming(AllocateCall, AllocOrInvokeContBB);

  Builder.CreateCall(CGM.getIntrinsic(llvm::Intrinsic::coro_begin),
                     {CoroId, Phi});

  // Make sure that we free the memory on any exceptions that happens
  // prior to the first suspend.
  struct CallCoroDelete final : public EHScopeStack::Cleanup {
    Stmt *S;
    void Emit(CodeGenFunction &CGF, Flags flags) override { CGF.EmitStmt(S); }
    CallCoroDelete(LabelStmt *LS) : S(LS->getSubStmt()) {}
  };
  EHStack.pushCleanup<CallCoroDelete>(EHCleanup, S.getDeallocate());

  EmitStmt(S.getPromiseDeclStmt());

  Address PromiseAddr = GetAddrOfLocalVar(S.getPromiseDecl());
  auto *PromiseAddrVoidPtr =
      new llvm::BitCastInst(PromiseAddr.getPointer(), VoidPtrTy, "", CoroId);
  // Update CoroId to refer to the promise. We could not do it earlier because
  // promise local variable was not emitted yet.
  CoroId->setArgOperand(1, PromiseAddrVoidPtr);

  // If SS.ResultDecl is not null, an object returned by get_return_object
  // requires a conversion to a return type. In this case, we declare at this
  // point and will emit a return statement at the end. Otherwise, emit return
  // statement here. Note, EmitReturnStmt omits branch to cleanup if current
  // function is a coroutine.
  if (S.getResultDecl()) {
    EmitStmt(S.getResultDecl());
  } else {
    EmitStmt(S.getReturnStmt());
  }

  // We will insert coro.end to cut any of the destructors for objects that
  // do not need to be destroyed onces the coroutine is resumed.
  struct CallCoroEnd final : public EHScopeStack::Cleanup {
    void Emit(CodeGenFunction &CGF, Flags flags) override {
      auto &CGM = CGF.CGM;
      auto NullPtr = llvm::ConstantPointerNull::get(CGF.Int8PtrTy);
      llvm::Function *CoroEnd = CGM.getIntrinsic(llvm::Intrinsic::coro_end);
      CGF.Builder.CreateCall(CoroEnd, {NullPtr, CGF.Builder.getInt1(true)});
    }
  };
  EHStack.pushCleanup<CallCoroEnd>(EHCleanup);

  // Body of the coroutine.
  {
    CodeGenFunction::RunCleanupsScope ResumeScope(*this);
    // TODO: Make sure that promise dtor is called.

    for (auto PM : S.getParamMoves()) {
      EmitStmt(PM);
      // TODO: if(CoroParam(...)) need to surround ctor and dtor
      // for the copy, so that llvm can elide it if the copy is
      // not needed.
    }

    CurCoro.Data->CurrentAwaitKind = AwaitKind::Init;
    EmitStmt(S.getInitSuspendStmt());

    CurCoro.Data->CurrentAwaitKind = AwaitKind::Normal;
    EmitStmt(S.getBody());

    if (Builder.GetInsertBlock()) {
      CurCoro.Data->CurrentAwaitKind = AwaitKind::Final;
      EmitStmt(S.getFinalSuspendStmt());
    }
  }
  EmitStmt(S.getDeallocate());

  EmitBlock(RetBB);
  llvm::Function *CoroEnd = CGM.getIntrinsic(llvm::Intrinsic::coro_end);
  Builder.CreateCall(CoroEnd, {NullPtr, Builder.getInt1(0)});

  // Emit return statement only if we are doing two stage return initialization.
  // I.e. when get_return_object requires a conversion to a return type.
  if (S.getResultDecl()) {
    EmitStmt(S.getReturnStmt());
  }

  runHorribleHackToFixupCleanupBlocks(*CurFn);
}
