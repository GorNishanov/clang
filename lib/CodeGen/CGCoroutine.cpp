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
                                    unsigned suspendNum,
                                    BasicBlock *returnBlock = nullptr) {
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

  if (auto InvokeDest = CGF.getInvokeDest()) {
    Builder.CreateInvoke(coroSuspend, ReadyBlock, InvokeDest, args);
  }
  else {
    llvm_unreachable("cannot handle yet");
  }
#if 0
  // FIXME: use invoke / landing pad model
  // EmitBranchThrough Cleanup does not generate as nice cleanup code
  auto suspendResult = Builder.CreateCall(coroSuspend, args);
  Builder.CreateCondBr(suspendResult, returnBlock ? returnBlock : ReadyBlock, cleanupBlock);
#if 0
  CGF.EmitBlock(cleanupBlock);

#if 1
  auto jumpDest = CGF.getJumpDestForLabel(CGF.getCGCoroutine().DeleteLabel);
  CGF.EmitBranchThroughCleanup(jumpDest);
#else
  // FIXME: switch to invoke / landing pad (was: CGF.EmitBranchThroughCleanup();)
  // EmitBranchThrough cleanup generates extra variables and switches that
  // confuse CoroSplit pass (and add unnecessary state to coroutine frame)

 // auto jumpDest = CGF.getJumpDestForLabel(CGF.getCGCoroutine().DeleteLabel);
  Builder.CreateBr(jumpDest.getBlock());
#endif
#endif
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
    : CGF(F), suspendNum(0) {}

clang::CodeGen::CGCoroutine::~CGCoroutine() {}

static void InitializeReturnSlot(CodeGenFunction& CGF, Expr const* RV) {
  CodeGenFunction::RunCleanupsScope cleanupScope(CGF);
  if (const ExprWithCleanups *cleanups =
    dyn_cast_or_null<ExprWithCleanups>(RV)) {
    CGF.enterFullExpression(cleanups);
    RV = cleanups->getSubExpr();
  }

  switch (CGF.getEvaluationKind(RV->getType())) {
  case TEK_Scalar:
    CGF.Builder.CreateStore(CGF.EmitScalarExpr(RV), CGF.ReturnValue);
    break;
  case TEK_Complex:
    CGF.EmitComplexExprIntoLValue(RV, CGF.MakeAddrLValue(CGF.ReturnValue, RV->getType()),
      /*isInit*/ true);
    break;
  case TEK_Aggregate:
    CGF.EmitAggExpr(RV, AggValueSlot::forAddr(CGF.ReturnValue,
      Qualifiers(),
      AggValueSlot::IsDestructed,
      AggValueSlot::DoesNotNeedGCBarriers,
      AggValueSlot::IsNotAliased));
    break;
  }
}

void CodeGenFunction::EmitCoroutineBody(const CoroutineBodyStmt &S) {
  auto &SS = S.getSubStmts();

  // allocate memory for coroutine frame
  Builder.CreateCall(CGM.getIntrinsic(llvm::Intrinsic::coro_init),
                     EmitScalarExpr(SS.Allocate));

  auto rampReturn = createBasicBlock("coro.ret");
  {
    CodeGenFunction::RunCleanupsScope ResumeScope(*this);
    EmitStmt(SS.Promise);

    auto start = createBasicBlock("coro.start");

    llvm::Function* coroDone = CGM.getIntrinsic(llvm::Intrinsic::coro_done);
    auto doneResult = Builder.CreateCall(coroDone, llvm::ConstantPointerNull::get(CGM.Int8PtrTy));
    Builder.CreateCondBr(doneResult, rampReturn, start);

    EmitBlock(start);
    getCGCoroutine().DeleteLabel = SS.Deallocate->getDecl();

    emitSuspendExpression(*this, Builder, *SS.InitSuspend,
      "init", 1);

    EmitStmt(SS.Body);

    if (Builder.GetInsertBlock()) {
      // create final block if we we don't have endless loop
      auto FinalBlock = createBasicBlock("coro.fin");
      EmitBlock(FinalBlock);
      emitSuspendExpression(*this, Builder, *SS.FinalSuspend,
        "final", 0);
    }
  }
  EmitStmt(SS.Deallocate);

  EmitBlock(rampReturn);
  //EmitStmt(SS.ResultDecl);
  EmitStmt(SS.ReturnStmt);
}
