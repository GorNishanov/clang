//===----- CGCoroData.cpp - Emit LLVM Code for C++ coroutines ------------===//
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
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Intrinsics.h>

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

  llvm::Function *coroSave = CGF.CGM.getIntrinsic(llvm::Intrinsic::coro_save);
  auto SaveCall = Builder.CreateCall(coroSave, {});

  // FIXME: Handle bool returning suspendExpr.
  CGF.EmitScalarExpr(S.getSuspendExpr());

  llvm::Function *coroSuspend =
      CGF.CGM.getIntrinsic(llvm::Intrinsic::coro_suspend);
  SmallVector<Value *, 1> args2{SaveCall, Builder.getInt1(IsFinalSuspend)};
  auto SuspendResult = Builder.CreateCall(coroSuspend, args2);
  auto Switch = Builder.CreateSwitch(SuspendResult, Coro.SuspendBB, 2);
  Switch->addCase(Builder.getInt8(0), ReadyBlock);
  Switch->addCase(Builder.getInt8(1), CleanupBlock);

  CGF.EmitBlock(CleanupBlock);

  // FIXME: This does not work if co_await exp result is used
  //   see clang/test/Coroutines/brokenIR.cpp
  auto jumpDest = CGF.getJumpDestForLabel(Coro.DeleteLabel);
  CGF.EmitBranchThroughCleanup(jumpDest);

  CGF.EmitBlock(ReadyBlock);
  return CGF.EmitScalarExpr(S.getResumeExpr());
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

static void fixUpDelete(CodeGenFunction& CGF) {
  // Converts this code:
  //   Whatever(coro_free(coro_frame()))
  // To this:
  //
  //   %0 = coro_free(coro_frame())
  //   if (%0) Whatever(%0)
  // Scans from the end of the block to find coro_frame and then adds what is 
  // needed.
  auto BB = CGF.Builder.GetInsertBlock();
  assert(!BB->empty());
  llvm::IntrinsicInst* CoroFree = nullptr;
  auto BI = &BB->back();
  auto BE = &BB->front();
  do {
    if (auto II = dyn_cast<llvm::IntrinsicInst>(BI))
      if (II->getIntrinsicID() == llvm::Intrinsic::coro_free) {
        CoroFree = II;
        break;
      }
    BI = BI->getPrevNode();
  } while (BI != BE);
  assert(CoroFree);

  auto EndBB =
    BasicBlock::Create(BB->getContext(), "EndBB", BB->getParent());
  CGF.Builder.CreateBr(EndBB);

  auto FreeBB = BB->splitBasicBlock(CoroFree->getNextNode(), "FreeBB");

  auto NullPtr = llvm::ConstantPointerNull::get(CGF.CGM.Int8PtrTy);

  CGF.Builder.SetInsertPoint(CoroFree->getNextNode());
  auto Cond = CGF.Builder.CreateICmpNE(CoroFree, NullPtr);
  CGF.Builder.CreateCondBr(Cond, FreeBB, EndBB);
  BB->getTerminator()->eraseFromParent();

  CGF.Builder.SetInsertPoint(EndBB);
}

void CodeGenFunction::EmitCoroutineBody(const CoroutineBodyStmt &S) {
  auto &SS = S.getSubStmts();
  auto NullPtr = llvm::ConstantPointerNull::get(Builder.getInt8PtrTy());

  auto CoroAlloc =
      Builder.CreateCall(CGM.getIntrinsic(llvm::Intrinsic::coro_alloc));
  auto ICmp = Builder.CreateICmpNE(CoroAlloc, NullPtr);

  auto EntryBB = Builder.GetInsertBlock();
  auto AllocBB = createBasicBlock("coro.alloc");
  auto InitBB = createBasicBlock("coro.init");
  auto RetBB = createBasicBlock("coro.ret");

  CurCoro.Data = std::unique_ptr<CGCoroData>(new CGCoroData);
  CurCoro.Data->SuspendBB = RetBB;
  CurCoro.Data->DeleteLabel = SS.Deallocate->getDecl();

  Builder.CreateCondBr(ICmp, InitBB, AllocBB);

  EmitBlock(AllocBB);

  auto AllocateCall = EmitScalarExpr(SS.Allocate);
  auto AllocOrInvokeContBB = Builder.GetInsertBlock();
  Builder.CreateBr(InitBB);

  EmitBlock(InitBB);
  auto Phi = Builder.CreatePHI(VoidPtrTy, 2);
  Phi->addIncoming(CoroAlloc, EntryBB);
  Phi->addIncoming(AllocateCall, AllocOrInvokeContBB);

  // We would like to insert coro_begin at this point, but we don't have an
  // alloca for the coroutine promise yet, which is one of the parameters for
  // coro_begin. So will will mark the spot and come back to it later.
  llvm::Value *Undef = llvm::UndefValue::get(Int32Ty);
  auto *CoroBeginInsertPt =
      new llvm::BitCastInst(Undef, Int32Ty, "CoroInitPt", InitBB);

  // Make sure that we free the memory on any exceptions that happens
  // prior to the first suspend.
  struct CallCoroDelete final : public EHScopeStack::Cleanup {
    Stmt *S;
    void Emit(CodeGenFunction &CGF, Flags flags) override {
      CGF.EmitStmt(S);
      fixUpDelete(CGF);
    }
    CallCoroDelete(LabelStmt *LS) : S(LS->getSubStmt()) {}
  };
  EHStack.pushCleanup<CallCoroDelete>(EHCleanup, SS.Deallocate);

  EmitStmt(SS.Promise);
  auto DS = cast<DeclStmt>(SS.Promise);
  auto VD = cast<VarDecl>(DS->getSingleDecl());

  DeclRefExpr PromiseRef(VD, false, VD->getType().getNonReferenceType(),
                         VK_LValue, SourceLocation());
  llvm::Value *PromiseAddr = EmitLValue(&PromiseRef).getPointer();
  llvm::Value *PromiseAddrVoidPtr =
      new llvm::BitCastInst(PromiseAddr, VoidPtrTy, "", CoroBeginInsertPt);
  // FIXME: Instead of 0, pass equivalent of alignas(maxalign_t).

  SmallVector<llvm::Value *, 5> args{Phi, CoroAlloc, Builder.getInt32(0),
                                     PromiseAddrVoidPtr, NullPtr};

  llvm::CallInst::Create(CGM.getIntrinsic(llvm::Intrinsic::coro_begin), args,
                         "", CoroBeginInsertPt);
  CoroBeginInsertPt->eraseFromParent();

  // If SS.ResultDecl is not null, an object returned by get_return_object 
  // requires a conversion to a return type. In this case, we declare at this
  // point and will emit a return statement at the end. Otherwise, emit return
  // statement here. Note, EmitReturnStmt omits branch to cleanup if current
  // function is a coroutine.
  if (SS.ResultDecl) {
    EmitStmt(SS.ResultDecl);
  } else {
    EmitStmt(SS.ReturnStmt);
  }

  // We will insert coro.end to cut any of the destructors for objects that
  // do not need to be destroyed onces the coroutine is resumed.
  struct CallCoroEnd final : public EHScopeStack::Cleanup {
    void Emit(CodeGenFunction &CGF, Flags flags) override {
      auto &CGM = CGF.CGM;
      llvm::Function *CoroEnd = CGM.getIntrinsic(llvm::Intrinsic::coro_end);
      CGF.Builder.CreateCall(CoroEnd, CGF.Builder.getInt1(true));
    }
  };
  EHStack.pushCleanup<CallCoroEnd>(EHCleanup);

  // Body of the coroutine.
  {
    CodeGenFunction::RunCleanupsScope ResumeScope(*this);
    // TODO: make sure that promise dtor is called

    for (auto PM : S.getParamMoves()) {
      EmitStmt(PM);
      // TODO: if(CoroParam(...)) need to suround ctor and dtor
      // for the copy, so that llvm can elide it if the copy is
      // not needed
    }

    CurCoro.Data->CurrentAwaitKind = AwaitKind::Init;
    EmitStmt(SS.InitialSuspend);

    CurCoro.Data->CurrentAwaitKind = AwaitKind::Normal;
    EmitStmt(SS.Body);

    if (Builder.GetInsertBlock()) {
      CurCoro.Data->CurrentAwaitKind = AwaitKind::Final;
      EmitStmt(SS.FinalSuspend);
    }
  }
  EmitStmt(SS.Deallocate);
  fixUpDelete(*this);

  EmitBlock(RetBB);
  llvm::Function *CoroEnd = CGM.getIntrinsic(llvm::Intrinsic::coro_end);
  Builder.CreateCall(CoroEnd, Builder.getInt1(0));

  // Emit return statement only if we are doing two stage return intialization.
  // I.e. when get_return_object requires a conversion to a return type.
  if (SS.ResultDecl) {
    EmitStmt(SS.ReturnStmt);
  }

  CurFn->addFnAttr(llvm::Attribute::Coroutine);
}
