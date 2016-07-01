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

#include "CGCoroutine.h"
#include "CodeGenFunction.h"
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/IntrinsicInst.h>
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/StmtCXX.h"

#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace CodeGen;

using llvm::Value;
using llvm::BasicBlock;
using llvm::ConstantInt;
using llvm::APInt;

clang::CodeGen::CGCoroutine *
clang::CodeGen::CGCoroutine::Create(CodeGenFunction &F) {
  return new CGCoroutine(F);
}

void clang::CodeGen::CGCoroutine::functionFinished() { delete this; }

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

    OpaqueValueMappings(CodeGenFunction &CGF, CoroutineSuspendExpr &S)
      : common(CGF.EmitMaterializeTemporaryExpr(
        cast<MaterializeTemporaryExpr>(S.getCommonExpr()))),
      o1(CGF, opaque(S.getReadyExpr()), common),
      o2(CGF, opaque(S.getSuspendExpr()), common),
      o3(CGF, opaque(S.getResumeExpr()), common) {}
  };
}

static Value *emitSuspendExpression(CodeGenFunction &CGF, CGBuilderTy &Builder,
                                    CoroutineSuspendExpr &S, StringRef Name,
                                    unsigned suspendNum) {
  SmallString<8> suffix(Name);
  if (suspendNum > 1) {
    Twine(suspendNum).toVector(suffix);
  }

  OpaqueValueMappings ovm(CGF, S);

  SmallString<16> buffer;
  BasicBlock *ReadyBlock = CGF.createBasicBlock((suffix + Twine(".ready")).toStringRef(buffer));
  buffer.clear();
  BasicBlock *SuspendBlock = CGF.createBasicBlock((suffix + Twine(".suspend")).toStringRef(buffer));
  buffer.clear();
  BasicBlock *cleanupBlock = CGF.createBasicBlock((suffix + Twine(".cleanup")).toStringRef(buffer));

  CodeGenFunction::RunCleanupsScope AwaitExprScope(CGF);

  CGF.EmitBranchOnBoolExpr(S.getReadyExpr(), ReadyBlock, SuspendBlock, 0);
  CGF.EmitBlock(SuspendBlock);
#if 0
  auto bitcast = Builder.CreateBitCast(ovm.common.getPointer(), CGF.VoidPtrTy);

  llvm::Function* coroSuspend = CGF.CGM.getIntrinsic(llvm::Intrinsic::coro_suspend);
  SmallVector<Value*, 2> args{bitcast, CGF.EmitScalarExpr(S.getSuspendExpr()),
    ConstantInt::get(CGF.Int32Ty, suspendNum)
  };
#endif
  llvm::Function* coroSave = CGF.CGM.getIntrinsic(llvm::Intrinsic::coro_save);
  SmallVector<Value*, 1> args{ llvm::ConstantInt::get(CGF.Builder.getInt1Ty(), suspendNum == 0) };
  auto SaveCall = Builder.CreateCall(coroSave, args);

  // FIXME: handle bool returning suspendExpr
  CGF.EmitScalarExpr(S.getSuspendExpr());

  llvm::Function* coroSuspend = CGF.CGM.getIntrinsic(llvm::Intrinsic::coro_suspend);
  SmallVector<Value *, 1> args2{
      SaveCall
      };
  auto SuspendResult = Builder.CreateCall(coroSuspend, args2);
  auto Cond = Builder.CreateICmp(llvm::CmpInst::ICMP_EQ, SuspendResult,
                                 Builder.getInt8(0));

  if (suspendNum == 0)
    Builder.CreateBr(cleanupBlock);
  else
    Builder.CreateCondBr(Cond, ReadyBlock, cleanupBlock);

  CGF.EmitBlock(cleanupBlock);

  auto jumpDest = CGF.getJumpDestForLabel(CGF.getCGCoroutine().DeleteLabel);
  CGF.EmitBranchThroughCleanup(jumpDest);

  CGF.EmitBlock(ReadyBlock);
  return CGF.EmitScalarExpr(S.getResumeExpr());
}

Value *clang::CodeGen::CGCoroutine::EmitCoawait(CoawaitExpr &S) {
  return emitSuspendExpression(CGF, CGF.Builder, S, "await", ++suspendNum);
}
Value *clang::CodeGen::CGCoroutine::EmitCoyield(CoyieldExpr &S) {
  return emitSuspendExpression(CGF, CGF.Builder, S, "yield", ++suspendNum);
}
clang::CodeGen::CGCoroutine::CGCoroutine(CodeGenFunction &F)
    : CGF(F), suspendNum(1) {}

clang::CodeGen::CGCoroutine::~CGCoroutine() {}

namespace {
  struct GetParamRef : public StmtVisitor<GetParamRef> {
  public:
    DeclRefExpr* Expr = nullptr;
    GetParamRef() { }
    void VisitDeclRefExpr(DeclRefExpr *E) {
      assert(Expr == nullptr && "multilple declref in param move");
      Expr = E;
    }
  };
}

static void EmitCoroParam(CodeGenFunction& CGF, DeclStmt* PM) {
  assert(PM->isSingleDecl());
  VarDecl* VD = static_cast<VarDecl*>(PM->getSingleDecl());
  Expr* InitExpr = VD->getInit();
  GetParamRef Visitor;
  Visitor.Visit(InitExpr);// InitExpr);
//  Visitor.TraverseStmtExpr(InitExpr);// InitExpr);
  assert(Visitor.Expr);
  auto DREOrig = cast<DeclRefExpr>(Visitor.Expr);

  DeclRefExpr DRE(VD, /* RefersToEnclosingVariableOrCapture= */false,
                  VD->getType(), VK_LValue, SourceLocation{});
  auto Orig = CGF.Builder.CreateBitCast(CGF.EmitLValue(DREOrig).getAddress(), CGF.VoidPtrTy);
  auto Copy = CGF.Builder.CreateBitCast(CGF.EmitLValue(&DRE).getAddress(), CGF.VoidPtrTy);
  SmallVector<Value *, 2> args{Orig.getPointer(), Copy.getPointer()};

//  auto CoroParam = CGF.CGM.getIntrinsic(llvm::Intrinsic::coro_param);
//  auto Call = CGF.Builder.CreateCall(CoroParam, args);
}

// Converts this code:
//   S(coro_free(coro_frame())
// To this:
//
//   %0 = coro_free(coro_frame())
//   if (%0) S(%0)
//   coro_end(%0)
// Scans from the end of the block to find coro_frame and then adds what is 
// needed
static void FixUpDelete(CodeGenFunction& CGF) {
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
  }
  while (BI != BE);
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
  llvm::Function *CoroEnd = CGF.CGM.getIntrinsic(llvm::Intrinsic::coro_end);
  CGF.Builder.CreateCall(CoroEnd, CoroFree);
}

void CodeGenFunction::EmitCoroutineBody(const CoroutineBodyStmt &S) {
  auto &SS = S.getSubStmts();
  auto NullPtr = llvm::ConstantPointerNull::get(Builder.getInt8PtrTy());

  auto CoroAlloc = Builder.CreateCall(CGM.getIntrinsic(llvm::Intrinsic::coro_alloc));
  auto ICmp = Builder.CreateICmpNE(CoroAlloc, NullPtr);

  auto EntryBB = Builder.GetInsertBlock();
  auto AllocBB = createBasicBlock("coro.alloc");
  auto InitBB = createBasicBlock("coro.init");
  auto ParamCleanupBB = createBasicBlock("coro.param.cleanup");

  Builder.CreateCondBr(ICmp, InitBB, AllocBB);

  EmitBlock(AllocBB);

  auto AllocateCall = EmitScalarExpr(SS.Allocate);
  auto AllocOrInvokeContBB = Builder.GetInsertBlock();
  Builder.CreateBr(InitBB);
  
  EmitBlock(InitBB);
  auto Phi = Builder.CreatePHI(VoidPtrTy, 2);
  Phi->addIncoming(CoroAlloc, EntryBB);
  Phi->addIncoming(AllocateCall, AllocOrInvokeContBB);

  // we would like to insert coro_init at this point, but
  // we don't have alloca for the coroutine promise yet, which 
  // is one of the parameters for coro_init
  llvm::Value *Undef = llvm::UndefValue::get(Int32Ty);
  llvm::Instruction *CoroInitInsertPt =
      new llvm::BitCastInst(Undef, Int32Ty, "CoroInitPt", InitBB);

  {
	CodeGenFunction::RunCleanupsScope ResumeScope(*this);

	struct CallCoroDelete final : public EHScopeStack::Cleanup {
		void Emit(CodeGenFunction &CGF, Flags flags) override {
			CGF.EmitStmt(S);
      FixUpDelete(CGF);
		}
		CallCoroDelete(LabelStmt* LS) : S(LS->getSubStmt()) {
		}
	private:
		//CoroutineBodyStmt::SubStmt& SS;
		Stmt* S;
	};
	EHStack.pushCleanup<CallCoroDelete>(EHCleanup, SS.Deallocate);

    EmitStmt(SS.Promise);
    auto DS = cast<DeclStmt>(SS.Promise);
    auto VD = cast<VarDecl>(DS->getSingleDecl());

    DeclRefExpr PromiseRef(VD, false, VD->getType().getNonReferenceType(),
      VK_LValue, SourceLocation());
    llvm::Value *PromiseAddr = EmitLValue(&PromiseRef).getPointer();

    llvm::Value* FnAddrVoidPtr = new llvm::BitCastInst(CurFn, VoidPtrTy, "", CoroInitInsertPt);
    llvm::Value* PromiseAddrVoidPtr = new llvm::BitCastInst(PromiseAddr, VoidPtrTy, "", CoroInitInsertPt);
    // FIXME: instead of 0, pass equivalnet of alignas(maxalign_t)
    //     enum { kMem, kAlloc, kAlign, kPromise, kInfo };

    SmallVector<llvm::Value*, 5> args{ 
      Phi, 
      CoroAlloc,
      Builder.getInt32(0), 
      PromiseAddrVoidPtr, 
      NullPtr };

    llvm::CallInst::Create(
      CGM.getIntrinsic(llvm::Intrinsic::coro_begin), args, "", CoroInitInsertPt);
    CoroInitInsertPt->eraseFromParent();

    for (auto PM : S.getParamMoves()) {
      EmitStmt(PM);
      // TODO: if(CoroParam(...)) need to suround ctor and dtor
      // for the copy, so that llvm can elide it if the copy is
      // not needed
    }
    getCGCoroutine().DeleteLabel = SS.Deallocate->getDecl();

    EmitStmt(SS.ReturnStmt);

    struct CallCoroEnd final : public EHScopeStack::Cleanup {
      void Emit(CodeGenFunction &CGF, Flags flags) override {
        auto &CGM = CGF.CGM;
        auto NullPtr = llvm::ConstantPointerNull::get(CGM.Int8PtrTy);
        llvm::Function *CoroEnd = CGM.getIntrinsic(llvm::Intrinsic::coro_end);
        CGF.Builder.CreateCall(CoroEnd, NullPtr);
      }
    };
    EHStack.pushCleanup<CallCoroEnd>(EHCleanup);
#if 0

    auto StartBlock = createBasicBlock("coro.start");
    llvm::Function* CoroFork = CGM.getIntrinsic(llvm::Intrinsic::experimental_coro_fork);
    auto ForkResult = Builder.CreateCall(CoroFork);
    Builder.CreateCondBr(ForkResult, ParamCleanupBB, StartBlock);

    EmitBlock(StartBlock);
#endif
    emitSuspendExpression(*this, Builder, *SS.InitSuspend, "init", 1);

    EmitStmt(SS.Body);

    if (Builder.GetInsertBlock()) {
      // create final block if we we don't have an endless loop
      auto FinalBlock = createBasicBlock("coro.fin");
      EmitBlock(FinalBlock);
      emitSuspendExpression(*this, Builder, *SS.FinalSuspend,
        "final", 0);
    }
  }
#if 0
  auto DeallocBB = createBasicBlock("coro.free");
  auto ReturnBB = ReturnBlock.getBlock();

  auto CoroDelete = Builder.CreateCall(CGM.getIntrinsic(llvm::Intrinsic::coro_delete),
    llvm::ConstantPointerNull::get(VoidPtrTy));
  auto ShouldDeallocate =
    Builder.CreateICmpNE(CoroDelete,
      llvm::ConstantPointerNull::get(VoidPtrTy));

  Builder.CreateCondBr(ShouldDeallocate, DeallocBB, ReturnBB);

  EmitBlock(DeallocBB);
#endif
  EmitStmt(SS.Deallocate);
  FixUpDelete(*this);

  llvm::Function* CoroReturn = CGM.getIntrinsic(llvm::Intrinsic::coro_return);
  Builder.CreateCall(CoroReturn, NullPtr);

#if 1
  EmitBranch(ParamCleanupBB);
  
  EmitBlock(ParamCleanupBB);
#endif

  CurFn->addFnAttr(llvm::Attribute::Coroutine);
}
