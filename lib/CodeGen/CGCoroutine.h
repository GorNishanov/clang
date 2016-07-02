//===-- CGCoroutine.h - Emit LLVM Code for C++ coroutines -------*- C++ -*-===//
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

#ifndef LLVM_CLANG_LIB_CODEGEN_CGCOROUTINE_H
#define LLVM_CLANG_LIB_CODEGEN_CGCOROUTINE_H

namespace llvm {
class BasicBlock;
class Value;
} // namespace llvm

namespace clang {
class VarDecl;
class CoawaitExpr;
class CoyieldExpr;
class LabelDecl;

namespace CodeGen {
class CodeGenFunction;

class CGCoroutine {
private:
  CodeGenFunction &CGF;
  unsigned suspendNum;

public:
  LabelDecl *DeleteLabel = nullptr;
  llvm::BasicBlock *SuspendBB = nullptr;

  static CGCoroutine *Create(CodeGenFunction &);
  void functionFinished();
  llvm::Value *EmitCoawait(CoawaitExpr &S);
  llvm::Value *EmitCoyield(CoyieldExpr &S);

private:
  CGCoroutine(CodeGenFunction &);
  ~CGCoroutine();
};
}
}
#endif
