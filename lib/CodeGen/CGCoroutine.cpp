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
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Value.h"

using namespace clang;
using namespace CodeGen;

namespace clang {
namespace CodeGen {

struct CGCoroData {
  // More members to come in the future patches

  // Stores the llvm.coro.id emitted in the function so that we can supply it
  // as the first argument to coro.begin, coro.alloc and coro.free intrinsics.
  // Note: llvm.coro.id returns a token that cannot be directly expressed in a
  // builtin.
  llvm::CallInst *CoroId = nullptr;
};
}
}
clang::CodeGen::CodeGenFunction::CGCoroInfo::CGCoroInfo() {}
CodeGenFunction::CGCoroInfo::~CGCoroInfo() {}

static void createCoroDataIfNeeded(CodeGenFunction::CGCoroInfo& CurCoro) {
  if (CurCoro.Data)
    return;

  CurCoro.Data = std::unique_ptr<CGCoroData>(new CGCoroData);
}

// Emit coroutine intrinsic and patch up arguments of the token type.
RValue CodeGenFunction::EmitCoroutineIntrinsic(const CallExpr *E,
                                               unsigned int IID) {
  SmallVector<llvm::Value*, 8> Args;
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
      "been used earlier in this function");
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

  // If we see @llvm.coro.id remember it. We will update coro.alloc, coro.begin
  // and coro.free intrinsics to refer to it.
  if (IID == llvm::Intrinsic::coro_id) {
    createCoroDataIfNeeded(CurCoro);

    if (CurCoro.Data->CoroId) {
      CGM.Error(E->getLocStart(),
                "only one __builtin_coro_id can be used in a function");
    }
    else {
      CurCoro.Data->CoroId = Call;
    }
  }

  return RValue::get(Call);
}
