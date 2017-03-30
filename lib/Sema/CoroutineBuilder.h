//===----- CoroutineBuilder.h - Coroutine Semantic checking -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//===----------------------------------------------------------------------===//
//
//  This file implements a semantic tree transformation that takes a given
//  AST and rebuilds it, possibly transforming some nodes in the process.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_SEMA_COROUTINEBUILDER_H
#define LLVM_CLANG_LIB_SEMA_COROUTINEBUILDER_H

#include "clang/AST/Decl.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/StmtCXX.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/SemaInternal.h"

namespace clang {

class CoroutineStmtBuilder : public CoroutineBodyStmt::CtorArgs {
  Sema &S;
  FunctionDecl &FD;
  sema::FunctionScopeInfo &Fn;
  bool IsValid = true;
  SourceLocation Loc;
  QualType RetType;
  SmallVector<Stmt *, 4> ParamMovesVector;
  const bool IsPromiseDependentType;
  CXXRecordDecl *PromiseRecordDecl = nullptr;

public:
  CoroutineStmtBuilder(Sema &S, FunctionDecl &FD, sema::FunctionScopeInfo &Fn,
                       Stmt *Body)
      : S(S), FD(FD), Fn(Fn), Loc(FD.getLocation()),
        IsPromiseDependentType(
            !Fn.CoroutinePromise ||
            Fn.CoroutinePromise->getType()->isDependentType()) {
    this->Body = Body;
    if (!IsPromiseDependentType) {
      PromiseRecordDecl = Fn.CoroutinePromise->getType()->getAsCXXRecordDecl();
      assert(PromiseRecordDecl && "Type should have already been checked");
    }
    this->IsValid = makePromiseStmt() && makeInitialAndFinalSuspend();
  }

  bool buildStatements() {
    assert(this->IsValid && "coroutine already invalid");
    this->IsValid = makeReturnObject() && makeParamMoves();
    if (this->IsValid && !IsPromiseDependentType)
      buildDependentStatements();
    return this->IsValid;
  }

  bool buildDependentStatements() {
    assert(this->IsValid && "coroutine already invalid");
    assert(!this->IsPromiseDependentType &&
           "coroutine cannot have a dependent promise type");
    this->IsValid = makeOnException() && makeOnFallthrough() &&
                    makeReturnOnAllocFailure() && makeNewAndDeleteExpr();
    return this->IsValid;
  }

  bool isInvalid() const { return !this->IsValid; }

private:
  bool makePromiseStmt();
  bool makeInitialAndFinalSuspend();
  bool makeNewAndDeleteExpr();
  bool makeOnFallthrough();
  bool makeOnException();
  bool makeReturnObject();
  bool makeReturnOnAllocFailure();
  bool makeParamMoves();
};

} // end namespace clang

#endif // LLVM_CLANG_LIB_SEMA_COROUTINEBUILDER_H
