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
#include "llvm/IR/Intrinsics.h"
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

    static OpaqueValueExpr *opaque(Expr *E) {
      return cast<OpaqueValueExpr>(
        cast<CXXMemberCallExpr>(E)->getImplicitObjectArgument());
    }

    OpaqueValueMappings(CodeGenFunction &CGF, CoroutineSuspendExpr &S)
      : common(CGF.EmitMaterializeTemporaryExpr(
        cast<MaterializeTemporaryExpr>(S.getCommonExpr()))),
      o1(CGF, opaque(S.getReadyExpr()), common),
      o2(CGF, opaque(S.getResumeExpr()), common) {}
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

  auto bitcast = Builder.CreateBitCast(ovm.common.getPointer(), CGF.VoidPtrTy);

  llvm::Function* coroSuspend = CGF.CGM.getIntrinsic(llvm::Intrinsic::coro_suspend);
  SmallVector<Value*, 2> args{bitcast, CGF.EmitScalarExpr(S.getSuspendExpr()),
    ConstantInt::get(CGF.Int32Ty, suspendNum)
  };

  auto suspendResult = Builder.CreateCall(coroSuspend, args);
  if (suspendNum == 0)
    Builder.CreateBr(cleanupBlock);
  else
    Builder.CreateCondBr(suspendResult, ReadyBlock, cleanupBlock);

  CGF.EmitBlock(cleanupBlock);

  auto jumpDest = CGF.getJumpDestForLabel(CGF.getCGCoroutine().DeleteLabel);
#if 1
  CGF.EmitBranchThroughCleanup(jumpDest);
#else
  // FIXME: switch to invoke / landing pad (was: CGF.EmitBranchThroughCleanup();)
  // EmitBranchThrough cleanup generates extra variables and switches that
  // confuse CoroSplit pass (and add unnecessary state to coroutine frame)

  Builder.CreateBr(jumpDest.getBlock());
#endif

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

void CodeGenFunction::EmitCoroutineBody(const CoroutineBodyStmt &S) {
  auto &SS = S.getSubStmts();

  auto BoolTy = llvm::Type::getInt1Ty(getLLVMContext());
  auto FlagAddr = Builder.CreateAlloca(BoolTy); // TODO: move it together with all others

  auto CoroElide = Builder.CreateCall(CGM.getIntrinsic(llvm::Intrinsic::coro_elide));
  auto ICmp =
    Builder.CreateICmpNE(CoroElide,
      llvm::ConstantPointerNull::get(VoidPtrTy));

  auto EntryBB = Builder.GetInsertBlock();
  auto AllocBB = createBasicBlock("coro.alloc");
  auto InitBB = createBasicBlock("coro.init");

  Builder.CreateCondBr(ICmp, InitBB, AllocBB);

  EmitBlock(AllocBB);

  auto AllocateCall = EmitScalarExpr(SS.Allocate);
  Builder.CreateBr(InitBB);
  
  EmitBlock(InitBB);
  auto Phi = Builder.CreatePHI(VoidPtrTy, 2);
  Phi->addIncoming(CoroElide, EntryBB);
  Phi->addIncoming(AllocateCall, AllocBB);

  Builder.CreateCall(CGM.getIntrinsic(llvm::Intrinsic::coro_init),
                         Phi);

  auto Select = Builder.CreateSelect(ICmp,
    llvm::ConstantInt::getTrue(BoolTy), llvm::ConstantInt::getFalse(BoolTy));
  Builder.CreateStore(Select, Address(FlagAddr,CharUnits::One()));

  {
    CodeGenFunction::RunCleanupsScope ResumeScope(*this);
    EmitStmt(SS.Promise);

    getCGCoroutine().DeleteLabel = SS.Deallocate->getDecl();

    EmitStmt(SS.ReturnStmt);

    auto StartBlock = createBasicBlock("coro.start");
    llvm::Function* coroDone = CGM.getIntrinsic(llvm::Intrinsic::coro_done);
    auto doneResult = Builder.CreateCall(coroDone, llvm::ConstantPointerNull::get(CGM.Int8PtrTy));
    Builder.CreateCondBr(doneResult, ReturnBlock.getBlock(), StartBlock);

    EmitBlock(StartBlock);
    emitSuspendExpression(*this, Builder, *SS.InitSuspend,
      "init", 1);

    EmitStmt(SS.Body);

    if (Builder.GetInsertBlock()) {
      // create final block if we we don't have an endless loop
      auto FinalBlock = createBasicBlock("coro.fin");
      EmitBlock(FinalBlock);
      emitSuspendExpression(*this, Builder, *SS.FinalSuspend,
        "final", 0);
    }
  }

  auto DeallocBB = createBasicBlock("coro.free");
  auto ReturnBB = ReturnBlock.getBlock();

  auto ShouldDeallocate = 
    Builder.CreateLoad(Address(FlagAddr, CharUnits::One()));
  Builder.CreateCondBr(ShouldDeallocate, DeallocBB, ReturnBB);

  EmitBlock(DeallocBB);
  EmitStmt(SS.Deallocate);
  EmitBranch(ReturnBlock.getBlock());

  CurFn->addFnAttr(llvm::Attribute::Coroutine);
}
