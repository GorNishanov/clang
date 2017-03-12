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
#include "llvm/ADT/ScopeExit.h"
#include "clang/AST/StmtCXX.h"

using namespace clang;
using namespace CodeGen;

using llvm::Value;
using llvm::BasicBlock;

namespace {
enum class AwaitKind { Init, Normal, Yield, Final };
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

// Synthesize a pretty name for a suspend point.
static SmallString<32> buildSuspendSuffixStr(CGCoroData &Coro, AwaitKind Kind) {
  unsigned No = 0;
  StringRef AwaitKindStr = 0;
  switch (Kind) {
  case AwaitKind::Init:
    AwaitKindStr = "init";
    break;
  case AwaitKind::Final:
    AwaitKindStr = "final";
    break;
  case AwaitKind::Normal:
    AwaitKindStr = "await";
    No = ++Coro.AwaitNum;
    break;
  case AwaitKind::Yield:
    AwaitKindStr = "yield";
    No = ++Coro.YieldNum;
    break;
  }
  SmallString<32> Suffix(AwaitKindStr);
  if (No > 1) {
    Twine(No).toVector(Suffix);
  }
  return Suffix;
}

// Emit suspend expression which roughly looks like:
//
//   auto && x = CommonExpr();
//   if (!x.await_ready()) {
//      llvm_coro_save();
//      x.await_suspend(...);     (*)
//      llvm_coro_suspend(); (**)
//   }
//   x.await_resume();
//
// where the result of the entire expression is the result of x.await_resume()
//
//   (*) If x.await_suspend return type is bool, it allows to veto a suspend:
//      if (x.await_suspend(...))
//        llvm_coro_suspend();
//
//  (**) llvm_coro_suspend() encodes three possible continuations as
//       a switch instruction:
//
//  %where-to = call i8 @llvm.coro.suspend(...)
//  switch i8 %where-to, label %coro.ret [ ; jump to epilogue to suspend
//    i8 0, label %yield.ready   ; go here when resumed
//    i8 1, label %yield.cleanup ; go here when destroyed
//  ]
//
//  See llvm's docs/Coroutines.rst for more details.
//
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

  auto Binder =
      CodeGenFunction::OpaqueValueMappingData::bind(CGF, S.getOpaqueValue(), E);
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
    assert(SuspendRet->getType()->isIntegerTy(1) &&
      "Sema should have already checked that it is void or bool");
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

void CodeGenFunction::EmitCoreturnStmt(CoreturnStmt const &S) {
  ++CurCoro.Data->CoreturnCount;
  EmitStmt(S.getPromiseCall());
  EmitBranchThroughCleanup(CurCoro.Data->FinalJD);
}

void CodeGenFunction::EmitCoroutineBody(const CoroutineBodyStmt &S) {
  auto *NullPtr = llvm::ConstantPointerNull::get(Builder.getInt8PtrTy());
  auto &TI = CGM.getContext().getTargetInfo();
  unsigned NewAlign = TI.getNewAlign() / TI.getCharWidth();

  auto *FinalBB = createBasicBlock("coro.final");
  auto *RetBB = createBasicBlock("coro.ret");

  auto *CoroId = Builder.CreateCall(
      CGM.getIntrinsic(llvm::Intrinsic::coro_id),
      {Builder.getInt32(NewAlign), NullPtr, NullPtr, NullPtr});
  createCoroData(*this, CurCoro, CoroId);
  CurCoro.Data->SuspendBB = RetBB;

  EmitScalarExpr(S.getAllocate());

  // FIXME: Setup cleanup scopes.

  EmitStmt(S.getPromiseDeclStmt());

  CurCoro.Data->CleanupJD = getJumpDestInCurrentScope(RetBB);
  CurCoro.Data->FinalJD = getJumpDestInCurrentScope(FinalBB);

  // FIXME: Emit initial suspend and more before the body.

  EmitStmt(S.getBody());

  // See if we need to generate final suspend.
  const bool CanFallthrough = Builder.GetInsertBlock();
  const bool HasCoreturns = CurCoro.Data->CoreturnCount > 0;
  if (CanFallthrough || HasCoreturns) {
    EmitBlock(FinalBB);
    // FIXME: Emit final suspend.
  }
  EmitStmt(S.getDeallocate());

  EmitBlock(RetBB);

  // FIXME: Emit return for the coroutine return object.
}

// Emit coroutine intrinsic and patch up arguments of the token type.
RValue CodeGenFunction::EmitCoroutineIntrinsic(const CallExpr *E,
                                               unsigned int IID) {
  SmallVector<llvm::Value *, 8> Args;
  switch (IID) {
  default:
    break;
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
  return RValue::get(Call);
}
