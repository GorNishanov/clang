//===--- SemaCoroutines.cpp - Semantic Analysis for Coroutines ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements semantic analysis for C++ Coroutines.
//
//===----------------------------------------------------------------------===//

#include "TreeTransform.h"
#include "clang/Sema/SemaInternal.h"
#include "clang/AST/Decl.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/StmtCXX.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Initialization.h"
#include "clang/Sema/Overload.h"
using namespace clang;
using namespace sema;

/// Look up the std::coroutine_traits<...>::promise_type for the given
/// function type.
static QualType lookupPromiseType(Sema &S, const FunctionProtoType *FnType,
                                  SourceLocation Loc) {
  // FIXME: Cache std::coroutine_traits once we've found it.
  NamespaceDecl *StdExp = S.lookupStdExperimentalNamespace();
  if (!StdExp) {
    S.Diag(Loc, diag::err_implied_std_coroutine_traits_not_found);
    return QualType();
  }

  LookupResult Result(S, &S.PP.getIdentifierTable().get("coroutine_traits"),
                      Loc, Sema::LookupOrdinaryName);
  if (!S.LookupQualifiedName(Result, StdExp)) {
    S.Diag(Loc, diag::err_implied_std_coroutine_traits_not_found);
    return QualType();
  }

  ClassTemplateDecl *CoroTraits = Result.getAsSingle<ClassTemplateDecl>();
  if (!CoroTraits) {
    Result.suppressDiagnostics();
    // We found something weird. Complain about the first thing we found.
    NamedDecl *Found = *Result.begin();
    S.Diag(Found->getLocation(), diag::err_malformed_std_coroutine_traits);
    return QualType();
  }

  // Form template argument list for coroutine_traits<R, P1, P2, ...>.
  TemplateArgumentListInfo Args(Loc, Loc);
  Args.addArgument(TemplateArgumentLoc(
      TemplateArgument(FnType->getReturnType()),
      S.Context.getTrivialTypeSourceInfo(FnType->getReturnType(), Loc)));
  // FIXME: If the function is a non-static member function, add the type
  // of the implicit object parameter before the formal parameters.
  for (QualType T : FnType->getParamTypes())
    Args.addArgument(TemplateArgumentLoc(
        TemplateArgument(T), S.Context.getTrivialTypeSourceInfo(T, Loc)));

  // Build the template-id.
  QualType CoroTrait =
      S.CheckTemplateIdType(TemplateName(CoroTraits), Loc, Args);
  if (CoroTrait.isNull())
    return QualType();
  if (S.RequireCompleteType(Loc, CoroTrait,
                            diag::err_coroutine_traits_missing_specialization))
    return QualType();

  CXXRecordDecl *RD = CoroTrait->getAsCXXRecordDecl();
  assert(RD && "specialization of class template is not a class?");

  // Look up the ::promise_type member.
  LookupResult R(S, &S.PP.getIdentifierTable().get("promise_type"), Loc,
                 Sema::LookupOrdinaryName);
  S.LookupQualifiedName(R, RD);
  auto *Promise = R.getAsSingle<TypeDecl>();
  if (!Promise) {
    S.Diag(Loc, diag::err_implied_std_coroutine_traits_promise_type_not_found)
        << RD;
    return QualType();
  }

  // The promise type is required to be a class type.
  QualType PromiseType = S.Context.getTypeDeclType(Promise);
  if (!PromiseType->getAsCXXRecordDecl()) {
    // Use the fully-qualified name of the type.
    auto *NNS = NestedNameSpecifier::Create(S.Context, nullptr, StdExp);
    NNS = NestedNameSpecifier::Create(S.Context, NNS, false,
                                      CoroTrait.getTypePtr());
    PromiseType = S.Context.getElaboratedType(ETK_None, NNS, PromiseType);

    S.Diag(Loc, diag::err_implied_std_coroutine_traits_promise_type_not_class)
        << PromiseType;
    return QualType();
  }

  return PromiseType;
}

static bool isValidCoroutineContext(Sema &S, SourceLocation Loc,
                                    StringRef Keyword) {
  // 'co_await' and 'co_yield' are not permitted in unevaluated operands.
  if (S.isUnevaluatedContext()) {
    S.Diag(Loc, diag::err_coroutine_unevaluated_context) << Keyword;
    return false;
  }

  // Any other usage must be within a function.
  auto *FD = dyn_cast<FunctionDecl>(S.CurContext);
  if (!FD) {
    S.Diag(Loc, isa<ObjCMethodDecl>(S.CurContext)
                    ? diag::err_coroutine_objc_method
                    : diag::err_coroutine_outside_function) << Keyword;
    return false;
  }

  // An enumeration for mapping the diagnostic type to the correct diagnostic
  // selection index.
  enum InvalidFuncDiag {
    DiagCtor = 0,
    DiagDtor,
    DiagCopyAssign,
    DiagMoveAssign,
    DiagMain,
    DiagConstexpr,
    DiagAutoRet,
    DiagVarargs,
  };
  bool Diagnosed = false;
  auto DiagInvalid = [&](InvalidFuncDiag ID) {
    S.Diag(Loc, diag::err_coroutine_invalid_func_context) << ID << Keyword;
    Diagnosed = true;
    return false;
  };

  // Diagnose when a constructor, destructor, copy/move assignment operator,
  // or the function 'main' are declared as a coroutine.
  auto *MD = dyn_cast<CXXMethodDecl>(FD);
  if (MD && isa<CXXConstructorDecl>(MD))
    return DiagInvalid(DiagCtor);
  else if (MD && isa<CXXDestructorDecl>(MD))
    return DiagInvalid(DiagDtor);
  else if (MD && MD->isCopyAssignmentOperator())
    return DiagInvalid(DiagCopyAssign);
  else if (MD && MD->isMoveAssignmentOperator())
    return DiagInvalid(DiagMoveAssign);
  else if (FD->isMain())
    return DiagInvalid(DiagMain);

  // Emit a diagnostics for each of the following conditions which is not met.
  if (FD->isConstexpr())
    DiagInvalid(DiagConstexpr);
  if (FD->getReturnType()->isUndeducedType())
    DiagInvalid(DiagAutoRet);
  if (FD->isVariadic())
    DiagInvalid(DiagVarargs);

  return !Diagnosed;
}

/// Check that this is a context in which a coroutine suspension can appear.
static FunctionScopeInfo *checkCoroutineContext(Sema &S, SourceLocation Loc,
                                                StringRef Keyword) {
  if (!isValidCoroutineContext(S, Loc, Keyword))
    return nullptr;

  assert(isa<FunctionDecl>(S.CurContext) && "not in a function scope");
  auto *FD = cast<FunctionDecl>(S.CurContext);
  auto *ScopeInfo = S.getCurFunction();
  assert(ScopeInfo && "missing function scope for function");

  // If we don't have a promise variable, build one now.
  if (!ScopeInfo->CoroutinePromise) {
    QualType T = FD->getType()->isDependentType()
                     ? S.Context.DependentTy
                     : lookupPromiseType(
                           S, FD->getType()->castAs<FunctionProtoType>(), Loc);
    if (T.isNull())
      return nullptr;

    // Create and default-initialize the promise.
    ScopeInfo->CoroutinePromise =
        VarDecl::Create(S.Context, FD, FD->getLocation(), FD->getLocation(),
                        &S.PP.getIdentifierTable().get("__promise"), T,
                        S.Context.getTrivialTypeSourceInfo(T, Loc), SC_None);
    S.CheckVariableDeclarationType(ScopeInfo->CoroutinePromise);
    if (!ScopeInfo->CoroutinePromise->isInvalidDecl())
      S.ActOnUninitializedDecl(ScopeInfo->CoroutinePromise, false);
  }

  return ScopeInfo;
}

static Expr *buildBuiltinCall(Sema &S, SourceLocation Loc, Builtin::ID Id,
                              MutableArrayRef<Expr *> CallArgs) {
  StringRef Name = S.Context.BuiltinInfo.getName(Id);
  LookupResult R(S, &S.Context.Idents.get(Name), Loc, Sema::LookupOrdinaryName);
  S.LookupName(R, S.TUScope, /*AllowBuiltinCreation=*/true);

  auto *BuiltInDecl = R.getAsSingle<FunctionDecl>();
  assert(BuiltInDecl && "failed to find builtin declaration");

  ExprResult DeclRef =
      S.BuildDeclRefExpr(BuiltInDecl, BuiltInDecl->getType(), VK_LValue, Loc);
  assert(DeclRef.isUsable() && "Builtin reference cannot fail");

  ExprResult Call =
      S.ActOnCallExpr(/*Scope=*/nullptr, DeclRef.get(), Loc, CallArgs, Loc);

  assert(!Call.isInvalid() && "Call to builtin cannot fail!");
  return Call.get();
}

static Expr *buildBuiltinCall(Sema &S, SourceLocation Loc, Builtin::ID id,
                              MutableArrayRef<Expr *> CallArgs) {
  StringRef Name = S.Context.BuiltinInfo.getName(id);
  LookupResult R(S, &S.Context.Idents.get(Name), Loc, Sema::LookupOrdinaryName);
  S.LookupName(R, S.TUScope, true);

  FunctionDecl *BuiltInDecl = R.getAsSingle<FunctionDecl>();
  assert(BuiltInDecl && "failed to find builtin declaration");

  ExprResult DeclRef = S.BuildDeclRefExpr(BuiltInDecl, BuiltInDecl->getType(),
                                          // S.Context.BuiltinFnTy,
                                          VK_RValue, Loc, nullptr);
  assert(DeclRef.isUsable() && "Builtin reference cannot fail");

  ExprResult Call =
      S.ActOnCallExpr(/*Scope=*/nullptr, DeclRef.get(), Loc, CallArgs, Loc);

  assert(!Call.isInvalid() && "Call to builtin cannot fail!");
  return Call.get();
}

/// Build a call to 'operator co_await' if there is a suitable operator for
/// the given expression.
static ExprResult buildOperatorCoawaitCall(Sema &SemaRef, Scope *S,
                                           SourceLocation Loc, Expr *E) {
  UnresolvedSet<16> Functions;
  SemaRef.LookupOverloadedOperatorName(OO_Coawait, S, E->getType(), QualType(),
                                       Functions);
  return SemaRef.CreateOverloadedUnaryOp(Loc, UO_Coawait, Functions, E);
}

struct ReadySuspendResumeResult {
  bool IsInvalid;
  Expr *Results[3];
};

static ExprResult buildMemberCall(Sema &S, Expr *Base, SourceLocation Loc,
                                  StringRef Name,
                                  MutableArrayRef<Expr *> Args) {
  DeclarationNameInfo NameInfo(&S.PP.getIdentifierTable().get(Name), Loc);

  // FIXME: Fix BuildMemberReferenceExpr to take a const CXXScopeSpec&.
  CXXScopeSpec SS;
  ExprResult Result = S.BuildMemberReferenceExpr(
      Base, Base->getType(), Loc, /*IsPtr=*/false, SS,
      SourceLocation(), nullptr, NameInfo, /*TemplateArgs=*/nullptr,
      /*Scope=*/nullptr);
  if (Result.isInvalid())
    return ExprError();

  return S.ActOnCallExpr(nullptr, Result.get(), Loc, Args, Loc, nullptr);
}

/// Build calls to await_ready, await_suspend, and await_resume for a co_await
/// expression.
static ReadySuspendResumeResult buildCoawaitCalls(Sema &S,
                                                  VarDecl *CoroutinePromise,
                                                  SourceLocation Loc, Expr *E) {
  // Assume invalid until we see otherwise.
  ReadySuspendResumeResult Calls = {true, {}};
  const size_t AwaitSuspendIndex = 1;

  const StringRef Funcs[] = {"await_ready", "await_suspend", "await_resume"};
  for (size_t I = 0, N = llvm::array_lengthof(Funcs); I != N; ++I) {
    Expr *Operand = new (S.Context) OpaqueValueExpr(Loc, E->getType(),
                                                    VK_LValue,
                                                    E->getObjectKind(), E);
    ExprResult Result;
    if (I == AwaitSuspendIndex) {
      QualType PromiseType = CoroutinePromise->getType();

      CXXScopeSpec SS;
      QualType CoroHandle = buildStdCoroutineHandle(S, PromiseType, Loc);
      if (CoroHandle.isNull())
        return Calls;

      if (S.RequireCompleteType(Loc, CoroHandle, diag::err_incomplete_type))
        return Calls;

      DeclContext *LookupCtx = S.computeDeclContext(CoroHandle);
      LookupResult Found(S, &S.PP.getIdentifierTable().get("from_address"), Loc,
                         Sema::LookupOrdinaryName);
      if (S.LookupQualifiedName(Found, LookupCtx)) {

        auto *Fn = Found.getAsSingle<CXXMethodDecl>();
        DeclRefExpr *DRE = DeclRefExpr::Create(
            S.Context, Fn->getQualifierLoc(), Loc, Fn,
            /*enclosing*/ false, Loc, Fn->getType(), VK_LValue);
        S.MarkDeclRefReferenced(DRE);

        Expr *FramePtr =
          buildBuiltinCall(S, Loc, Builtin::BI__builtin_coro_frame, {});

        ExprResult CoroHandle =
          S.ActOnCallExpr(/*Scope=*/nullptr, DRE, Loc, { FramePtr }, Loc);

        SmallVector<Expr*, 1> Args;
        Args.push_back(CoroHandle.get());
        Result = buildMemberCall(S, Operand, Loc, Funcs[I], Args);

        if (Result.isInvalid())
          return Calls;
      }
    } else {
      Result = buildMemberCall(S, Operand, Loc, Funcs[I], {});
    }
    if (Result.isInvalid())
      return Calls;
    Calls.Results[I] = Result.get();
  }

  Calls.IsInvalid = false;
  return Calls;
}

ExprResult Sema::ActOnCoawaitExpr(Scope *S, SourceLocation Loc, Expr *E) {
  auto *Coroutine = checkCoroutineContext(*this, Loc, "co_await");
  if (!Coroutine) {
    CorrectDelayedTyposInExpr(E);
    return ExprError();
  }
  if (E->getType()->isPlaceholderType()) {
    ExprResult R = CheckPlaceholderExpr(E);
    if (R.isInvalid()) return ExprError();
    E = R.get();
  }

  ExprResult Awaitable = buildOperatorCoawaitCall(*this, S, Loc, E);
  if (Awaitable.isInvalid())
    return ExprError();

  return BuildCoawaitExpr(Loc, Awaitable.get());
}
ExprResult Sema::BuildCoawaitExpr(SourceLocation Loc, Expr *E) {
  auto *Coroutine = checkCoroutineContext(*this, Loc, "co_await");
  if (!Coroutine)
    return ExprError();

  if (E->getType()->isPlaceholderType()) {
    ExprResult R = CheckPlaceholderExpr(E);
    if (R.isInvalid()) return ExprError();
    E = R.get();
  }

  if (E->getType()->isDependentType()) {
    Expr *Res = new (Context) CoawaitExpr(Loc, Context.DependentTy, E);
    Coroutine->CoroutineStmts.push_back(Res);
    return Res;
  }

  // If the expression is a temporary, materialize it as an lvalue so that we
  // can use it multiple times.
  if (E->getValueKind() == VK_RValue)
    E = CreateMaterializeTemporaryExpr(E->getType(), E, true);

  // Build the await_ready, await_suspend, await_resume calls.
  ReadySuspendResumeResult RSS =
      buildCoawaitCalls(*this, Coroutine->CoroutinePromise, Loc, E);
  if (RSS.IsInvalid)
    return ExprError();

  Expr *Res = new (Context) CoawaitExpr(Loc, E, RSS.Results[0], RSS.Results[1],
                                        RSS.Results[2]);
  Coroutine->CoroutineStmts.push_back(Res);
  return Res;
}

static ExprResult buildPromiseCall(Sema &S, FunctionScopeInfo *Coroutine,
                                   SourceLocation Loc, StringRef Name,
                                   MutableArrayRef<Expr *> Args) {
  assert(Coroutine->CoroutinePromise && "no promise for coroutine");

  // Form a reference to the promise.
  auto *Promise = Coroutine->CoroutinePromise;
  ExprResult PromiseRef = S.BuildDeclRefExpr(
      Promise, Promise->getType().getNonReferenceType(), VK_LValue, Loc);
  if (PromiseRef.isInvalid())
    return ExprError();

  // Call 'yield_value', passing in E.
  return buildMemberCall(S, PromiseRef.get(), Loc, Name, Args);
}

ExprResult Sema::ActOnCoyieldExpr(Scope *S, SourceLocation Loc, Expr *E) {
  auto *Coroutine = checkCoroutineContext(*this, Loc, "co_yield");
  if (!Coroutine) {
    CorrectDelayedTyposInExpr(E);
    return ExprError();
  }

  return BuildCoyieldExpr(Loc, E);
}
ExprResult Sema::BuildCoyieldExpr(SourceLocation Loc, Expr *E) {
  auto *Coroutine = checkCoroutineContext(*this, Loc, "co_yield");
  if (!Coroutine)
    return ExprError();

  if (E->getType()->isPlaceholderType() &&
      !E->getType()->isSpecificPlaceholderType(BuiltinType::Overload)) {
    ExprResult R = CheckPlaceholderExpr(E);
    if (R.isInvalid()) return ExprError();
    E = R.get();
  }

  if (E->getType()->isDependentType()) {
    Expr *Res = new (Context) CoyieldExpr(Loc, Context.DependentTy, E);
    Coroutine->CoroutineStmts.push_back(Res);
    return Res;
  }

  // FIXME: Moved it from ActOnCoyieldExpr to BuildCoyieldExpr, otherwise,
  //   mutlishot_func.cpp breaks. There must be a better fix.
  // Build yield_value call.
  ExprResult Awaitable =
      buildPromiseCall(*this, Coroutine, Loc, "yield_value", E);
  if (Awaitable.isInvalid())
    return ExprError();
  E = Awaitable.get();

  // Build 'operator co_await' call.
  Awaitable = buildOperatorCoawaitCall(*this, getCurScope(), Loc, E);
  if (Awaitable.isInvalid())
    return ExprError();

  // If the expression is a temporary, materialize it as an lvalue so that we
  // can use it multiple times.
  if (E->getValueKind() == VK_RValue)
    E = CreateMaterializeTemporaryExpr(E->getType(), E, true);

  // Build the await_ready, await_suspend, await_resume calls.
  ReadySuspendResumeResult RSS =
      buildCoawaitCalls(*this, Coroutine->CoroutinePromise, Loc, E);
  if (RSS.IsInvalid)
    return ExprError();

  Expr *Res = new (Context) CoyieldExpr(Loc, E, RSS.Results[0], RSS.Results[1],
                                        RSS.Results[2]);
  Coroutine->CoroutineStmts.push_back(Res);
  return Res;
}

StmtResult Sema::ActOnCoreturnStmt(SourceLocation Loc, Expr *E) {
  auto *Coroutine = checkCoroutineContext(*this, Loc, "co_return");
  if (!Coroutine) {
    CorrectDelayedTyposInExpr(E);
    return StmtError();
  }
  return BuildCoreturnStmt(Loc, E);
}

StmtResult Sema::BuildCoreturnStmt(SourceLocation Loc, Expr *E) {
  auto *Coroutine = checkCoroutineContext(*this, Loc, "co_return");
  if (!Coroutine)
    return StmtError();

  if (E && E->getType()->isPlaceholderType() &&
      !E->getType()->isSpecificPlaceholderType(BuiltinType::Overload)) {
    ExprResult R = CheckPlaceholderExpr(E);
    if (R.isInvalid()) return StmtError();
    E = R.get();
  }

  // FIXME: If the operand is a reference to a variable that's about to go out
  // of scope, we should treat the operand as an xvalue for this overload
  // resolution.
  ExprResult PC;
  if (E && (isa<InitListExpr>(E) || !E->getType()->isVoidType())) {
    PC = buildPromiseCall(*this, Coroutine, Loc, "return_value", E);
  } else {
    E = MakeFullDiscardedValueExpr(E).get();
    PC = buildPromiseCall(*this, Coroutine, Loc, "return_void", None);
  }
  if (PC.isInvalid())
    return StmtError();

  Expr *PCE = ActOnFinishFullExpr(PC.get()).get();

  Stmt *Res = new (Context) CoreturnStmt(Loc, E, PCE);
  Coroutine->CoroutineStmts.push_back(Res);
  return Res;
}

static ExprResult buildStdCurrentExceptionCall(Sema &S, SourceLocation Loc) {
  NamespaceDecl *Std = S.getStdNamespace();
  if (!Std) {
    S.Diag(Loc, diag::err_implied_std_current_exception_not_found);
    return ExprError();
  }
  LookupResult Result(S, &S.PP.getIdentifierTable().get("current_exception"),
                      Loc, Sema::LookupOrdinaryName);
  if (!S.LookupQualifiedName(Result, Std)) {
    S.Diag(Loc, diag::err_implied_std_current_exception_not_found);
    return ExprError();
  }

  // FIXME The STL is free to provide more than one overload.
  FunctionDecl *FD = Result.getAsSingle<FunctionDecl>();
  if (!FD) {
    S.Diag(Loc, diag::err_malformed_std_current_exception);
    return ExprError();
  }
  ExprResult Res = S.BuildDeclRefExpr(FD, FD->getType(), VK_LValue, Loc);
  Res = S.ActOnCallExpr(/*Scope*/ nullptr, Res.get(), Loc, None, Loc);
  if (Res.isInvalid()) {
    S.Diag(Loc, diag::err_malformed_std_current_exception);
    return ExprError();
  }
  return Res;
}

// Find an appropriate delete for the promise.
static FunctionDecl *findDeleteForPromise(Sema &S, SourceLocation Loc,
                                          QualType PromiseType) {
  FunctionDecl *OperatorDelete = nullptr;

  DeclarationName DeleteName =
      S.Context.DeclarationNames.getCXXOperatorName(OO_Delete);

  auto *PointeeRD = PromiseType->getAsCXXRecordDecl();
  assert(PointeeRD && "PromiseType must be a CxxRecordDecl type");

  if (S.FindDeallocationFunction(Loc, PointeeRD, DeleteName, OperatorDelete))
    return nullptr;

  if (!OperatorDelete) {
    // Look for a global declaration.
    const bool CanProvideSize = S.isCompleteType(Loc, PromiseType);
    const bool Overaligned = false;
    OperatorDelete = S.FindUsualDeallocationFunction(Loc, CanProvideSize,
                                                     Overaligned, DeleteName);
  }
  S.MarkFunctionReferenced(Loc, OperatorDelete);
  return OperatorDelete;
}

// Builds allocation and deallocation for the coroutine. Returns false on
// failure.
static bool buildAllocationAndDeallocation(Sema &S, SourceLocation Loc,
                                           FunctionScopeInfo *Fn,
                                           Expr *&Allocation,
                                           Stmt *&Deallocation) {
  TypeSourceInfo *TInfo = Fn->CoroutinePromise->getTypeSourceInfo();
  QualType PromiseType = TInfo->getType();
  if (PromiseType->isDependentType())
    return true;

  if (S.RequireCompleteType(Loc, PromiseType, diag::err_incomplete_type))
    return false;

  // FIXME: Add support for get_return_object_on_allocation failure.
  // FIXME: Add support for stateful allocators.

  FunctionDecl *OperatorNew = nullptr;
  FunctionDecl *OperatorDelete = nullptr;
  FunctionDecl *UnusedResult = nullptr;
  bool PassAlignment = false;

  S.FindAllocationFunctions(Loc, SourceRange(),
                            /*UseGlobal*/ false, PromiseType,
                            /*isArray*/ false, PassAlignment,
                            /*PlacementArgs*/ None, OperatorNew, UnusedResult);

  OperatorDelete = findDeleteForPromise(S, Loc, PromiseType);

  if (!OperatorDelete || !OperatorNew)
    return false;

  Expr *FramePtr =
      buildBuiltinCall(S, Loc, Builtin::BI__builtin_coro_frame, {});

  Expr *FrameSize =
      buildBuiltinCall(S, Loc, Builtin::BI__builtin_coro_size, {});

  // Make new call.

  ExprResult NewRef =
      S.BuildDeclRefExpr(OperatorNew, OperatorNew->getType(), VK_LValue, Loc);
  if (NewRef.isInvalid())
    return false;

  ExprResult NewExpr =
      S.ActOnCallExpr(S.getCurScope(), NewRef.get(), Loc, FrameSize, Loc);
  if (NewExpr.isInvalid())
    return false;

  Allocation = NewExpr.get();

  // Make delete call.

  QualType OpDeleteQualType = OperatorDelete->getType();

  ExprResult DeleteRef =
      S.BuildDeclRefExpr(OperatorDelete, OpDeleteQualType, VK_LValue, Loc);
  if (DeleteRef.isInvalid())
    return false;

  Expr *CoroFree =
      buildBuiltinCall(S, Loc, Builtin::BI__builtin_coro_free, {FramePtr});

  SmallVector<Expr *, 2> DeleteArgs{CoroFree};

  // Check if we need to pass the size.
  const auto *OpDeleteType =
      OpDeleteQualType.getTypePtr()->getAs<FunctionProtoType>();
  if (OpDeleteType->getNumParams() > 1)
    DeleteArgs.push_back(FrameSize);

  ExprResult DeleteExpr =
      S.ActOnCallExpr(S.getCurScope(), DeleteRef.get(), Loc, DeleteArgs, Loc);
  if (DeleteExpr.isInvalid())
    return false;

  Deallocation = DeleteExpr.get();

  return true;
}


struct RewriteParams : TreeTransform<RewriteParams> {
  typedef TreeTransform<RewriteParams> BaseTransform;

  ArrayRef<ParmVarDecl *> Params;
  ArrayRef<Stmt *> ParamsMove;
  RewriteParams(Sema &SemaRef, ArrayRef<ParmVarDecl *> P, ArrayRef<Stmt *> PM)
      : BaseTransform(SemaRef), Params(P), ParamsMove(PM) {}

  ExprResult TransformDeclRefExpr(DeclRefExpr *E) {
    ValueDecl *D = E->getDecl();
    if (auto *PD = dyn_cast<ParmVarDecl>(D)) {
      auto it = std::find(Params.begin(), Params.end(), PD);
      if (it != Params.end()) {
        size_t N = it - Params.begin();
        auto *Copy = cast<DeclStmt>(ParamsMove[N]);
        auto *VD = cast<VarDecl>(Copy->getSingleDecl());
        return SemaRef.BuildDeclRefExpr(
            VD, VD->getType(), ExprValueKind::VK_LValue, SourceLocation{});
      }
    }
    return BaseTransform::TransformDeclRefExpr(E);
  }
};
}

static ExprResult buildStdCurrentExceptionCall(Sema &S, SourceLocation Loc) {
  NamespaceDecl *Std = S.getStdNamespace();
  if (!Std) {
    S.Diag(Loc, diag::err_implied_std_current_exception_not_found);
    return ExprError();
  }
  LookupResult Result(S, &S.PP.getIdentifierTable().get("current_exception"),
    Loc, Sema::LookupOrdinaryName);
  if (!S.LookupQualifiedName(Result, Std)) {
    S.Diag(Loc, diag::err_implied_std_current_exception_not_found);
    return ExprError();
  }

  // FIXME The STL is free to provide more than one overload.
  FunctionDecl *FD = Result.getAsSingle<FunctionDecl>();
  if (!FD) {
    S.Diag(Loc, diag::err_malformed_std_current_exception);
    return ExprError();
  }
  ExprResult Res = S.BuildDeclRefExpr(FD, FD->getType(), VK_LValue, Loc);
  Res = S.ActOnCallExpr(/*Scope*/ nullptr, Res.get(), Loc, None, Loc);
  if (Res.isInvalid()) {
    S.Diag(Loc, diag::err_malformed_std_current_exception);
    return ExprError();
  }
  return Res;
}

namespace {
struct SubStmtBuilder {
  Stmt *Body = nullptr;
  Stmt *Promise = nullptr;
  Expr *InitialSuspend = nullptr;
  Expr *FinalSuspend = nullptr;
  Stmt *OnException = nullptr;
  Stmt *OnFallthrough = nullptr;
  Expr *Allocate = nullptr;
  Expr *Deallocate = nullptr;
  Stmt *ResultDecl = nullptr;
  Stmt *ReturnStmt = nullptr;

private:
  Sema &S;
  FunctionDecl &FD;
  FunctionScopeInfo &Fn;
  bool IsValid;
  SourceLocation Loc;
  QualType RetType;
  VarDecl *RetDecl = nullptr;
  SmallVector<Stmt *, 4> ParamMoves;
  SmallVector<ParmVarDecl *, 4> Params;

public:
  SubStmtBuilder(Sema &S, FunctionDecl &FD, FunctionScopeInfo &Fn, Stmt *Body)
      : S(S), FD(FD), Fn(Fn), Loc(FD.getLocation()) {
    this->Body = Body;
    this->IsValid = makePromiseStmt() && makeInitialSuspend() &&
                    makeFinalSuspend() && makeOnException() &&
                    makeOnFallthrough() && makeNewAndDeleteExpr() &&
                    makeResultDecl() && makeReturnStmt() && makeParamMoves() &&
                    makeBody();
    if (IsValid) {
#if 0
      // FIXME: parameter handling needs more work.
      //   Disabled the following code since
      //   buildCoyield/rebuildCoyield/actOnCoyield are not exactly right and
      //   cause multishot_func.cpp test to fail.
      RewriteParams RP(S, getParams(), getParamMoves());
      auto NewBody = RP.TransformStmt(Body);
      if (NewBody.isInvalid()) {
        IsValid = false;
        return;
      }
      this->Body = NewBody.get();
#endif
    }
  }

  bool makeBody() {
    if (!OnException)
      return true;

    StmtResult CatchBlock = S.ActOnCXXCatchBlock(Loc, nullptr, OnException);
    if (CatchBlock.isInvalid())
      return false;

    StmtResult TryBlock = S.ActOnCXXTryBlock(Loc, Body, {CatchBlock.get()});
    if (TryBlock.isInvalid())
      return false;

    Body = TryBlock.get();

    return true;
  }

  bool isInvalid() const { return !this->IsValid; }

  ArrayRef<Stmt *> getParamMoves() { return ParamMoves; }
  ArrayRef<ParmVarDecl *> getParams() { return Params; }

  bool makePromiseStmt() {
    // Form a declaration statement for the promise declaration, so that AST
    // visitors can more easily find it.
    StmtResult PromiseStmt = S.ActOnDeclStmt(
        S.ConvertDeclToDeclGroup(Fn.CoroutinePromise), Loc, Loc);
    if (PromiseStmt.isInvalid())
      return false;

    this->Promise = PromiseStmt.get();
    return true;
  }

  bool makeInitialSuspend() {
    // Form and check implicit 'co_await p.initial_suspend();' statement.
    ExprResult InitialSuspend =
        buildPromiseCall(S, &Fn, Loc, "initial_suspend", None);
    // FIXME: Support operator co_await here.
    if (!InitialSuspend.isInvalid())
      InitialSuspend = S.BuildCoawaitExpr(Loc, InitialSuspend.get());
    InitialSuspend = S.ActOnFinishFullExpr(InitialSuspend.get());
    if (InitialSuspend.isInvalid())
      return false;

    this->InitialSuspend = InitialSuspend.get();
    return true;
  }

  bool makeFinalSuspend() {
    // Form and check implicit 'co_await p.final_suspend();' statement.
    ExprResult FinalSuspend =
        buildPromiseCall(S, &Fn, Loc, "final_suspend", None);
    // FIXME: Support operator co_await here.
    if (!FinalSuspend.isInvalid())
      FinalSuspend = S.BuildCoawaitExpr(Loc, FinalSuspend.get());
    FinalSuspend = S.ActOnFinishFullExpr(FinalSuspend.get());
    if (FinalSuspend.isInvalid())
      return false;

  // Form and check allocation and deallocation calls.
  Expr *Allocation = nullptr;
  Stmt *Deallocation = nullptr;
  if (!buildAllocationAndDeallocation(*this, Loc, Fn, Allocation, Deallocation))
    return FD->setInvalidDecl();

    // If exceptions are disabled, don't try to build OnException.
  // Also try to form 'p.set_exception(std::current_exception());' to handle
  // uncaught exceptions.
  ExprResult SetException;
  StmtResult Fallthrough;
  if (Fn->CoroutinePromise &&
      !Fn->CoroutinePromise->getType()->isDependentType()) {
    CXXRecordDecl *RD = Fn->CoroutinePromise->getType()->getAsCXXRecordDecl();
    assert(RD && "Type should have already been checked");
    // [dcl.fct.def.coroutine]/4
    // The unqualified-ids 'return_void' and 'return_value' are looked up in
    // the scope of class P. If both are found, the program is ill-formed.
    DeclarationName RVoidDN = PP.getIdentifierInfo("return_void");
    LookupResult RVoidResult(*this, RVoidDN, Loc, Sema::LookupMemberName);
    const bool HasRVoid = LookupQualifiedName(RVoidResult, RD);

    DeclarationName RValueDN = PP.getIdentifierInfo("return_value");
    LookupResult RValueResult(*this, RValueDN, Loc, Sema::LookupMemberName);
    const bool HasRValue = LookupQualifiedName(RValueResult, RD);

    if (HasRVoid && HasRValue) {
      // FIXME Improve this diagnostic
      Diag(FD->getLocation(), diag::err_coroutine_promise_return_ill_formed)
          << RD;
      return FD->setInvalidDecl();
    } else if (HasRVoid) {
      // If the unqualified-id return_void is found, flowing off the end of a
      // coroutine is equivalent to a co_return with no operand. Otherwise,
      // flowing off the end of a coroutine results in undefined behavior.
      Fallthrough = BuildCoreturnStmt(FD->getLocation(), nullptr);
      Fallthrough = ActOnFinishFullStmt(Fallthrough.get());
      if (Fallthrough.isInvalid())
        return FD->setInvalidDecl();
    }

    // [dcl.fct.def.coroutine]/3
    // The unqualified-id set_exception is found in the scope of P by class
    // member access lookup (3.4.5).
    DeclarationName SetExDN = PP.getIdentifierInfo("set_exception");
    LookupResult SetExResult(*this, SetExDN, Loc, Sema::LookupMemberName);
    if (LookupQualifiedName(SetExResult, RD)) {
      // Form the call 'p.set_exception(std::current_exception())'
      SetException = buildStdCurrentExceptionCall(*this, Loc);
      if (SetException.isInvalid())
        return FD->setInvalidDecl();
      Expr *E = SetException.get();
      SetException = buildPromiseCall(*this, Fn, Loc, "set_exception", E);
      SetException = ActOnFinishFullExpr(SetException.get(), Loc);
      if (SetException.isInvalid())
        return FD->setInvalidDecl();
    }
  }

    if (!Fn.CoroutinePromise ||
      Fn.CoroutinePromise->getType()->isDependentType())
      return true;

    // [dcl.fct.def.coroutine]/3
    // The unqualified-id set_exception is found in the scope of P by class
    // member access lookup (3.4.5).
    DeclarationName SetExDN = S.PP.getIdentifierInfo("set_exception");
    LookupResult SetExResult(S, SetExDN, Loc, Sema::LookupMemberName);
    CXXRecordDecl *RD = Fn.CoroutinePromise->getType()->getAsCXXRecordDecl();
    assert(RD && "Type should have already been checked");

    if (S.LookupQualifiedName(SetExResult, RD)) {
      // Form the call 'p.set_exception(std::current_exception())'
      auto SetException = buildStdCurrentExceptionCall(S, Loc);
      if (SetException.isInvalid())
        return false;
      Expr *E = SetException.get();
      SetException = buildPromiseCall(S, &Fn, Loc, "set_exception", E);
      SetException = S.ActOnFinishFullExpr(SetException.get(), Loc);
      if (SetException.isInvalid())
        return false;
      this->OnException = SetException.get();
    }
    return true;
  }

  bool makeOnFallthrough() {
    if (!Fn.CoroutinePromise ||
        Fn.CoroutinePromise->getType()->isDependentType())
      return true;

    // [dcl.fct.def.coroutine]/4
    // The unqualified-ids 'return_void' and 'return_value' are looked up in
    // the scope of class P. If both are found, the program is ill-formed.
    DeclarationName RVoidDN = S.PP.getIdentifierInfo("return_void");
    LookupResult RVoidResult(S, RVoidDN, Loc, Sema::LookupMemberName);
    CXXRecordDecl *RD = Fn.CoroutinePromise->getType()->getAsCXXRecordDecl();
    const bool HasRVoid = S.LookupQualifiedName(RVoidResult, RD);

    DeclarationName RValueDN = S.PP.getIdentifierInfo("return_value");
    LookupResult RValueResult(S, RValueDN, Loc, Sema::LookupMemberName);
    const bool HasRValue = S.LookupQualifiedName(RValueResult, RD);

    if (HasRVoid && HasRValue) {
      // FIXME Improve this diagnostic
      S.Diag(FD.getLocation(), diag::err_coroutine_promise_return_ill_formed)
        << RD;
      return false;
    }
    else if (HasRVoid) {
      // If the unqualified-id return_void is found, flowing off the end of a
      // coroutine is equivalent to a co_return with no operand. Otherwise,
      // flowing off the end of a coroutine results in undefined behavior.
      StmtResult Fallthrough = S.BuildCoreturnStmt(FD.getLocation(), nullptr);
      Fallthrough = S.ActOnFinishFullStmt(Fallthrough.get());
      if (Fallthrough.isInvalid())
        return false;
      this->OnFallthrough = Fallthrough.get();
    }
    return true;
  }

  bool makeNewAndDeleteExpr() {
    TypeSourceInfo *TInfo = Fn.CoroutinePromise->getTypeSourceInfo();
    QualType PromiseType = TInfo->getType();

    FunctionDecl *OperatorNew = nullptr;
    FunctionDecl *OperatorDelete = nullptr;
    bool PassAlignment = false;

    S.FindAllocationFunctions(Loc, SourceRange(),
                              /*UseGlobal*/ false, PromiseType,
                              /*isArray*/ false, PassAlignment,
                              /*PlacementArgs*/ None, OperatorNew,
                              OperatorDelete);

    assert(OperatorNew && "we need to find at least global operator new");
    assert(OperatorDelete && "we need to find at least global operator new");

    Expr *FramePtr =
        buildBuiltinCall(S, Loc, Builtin::BI__builtin_coro_frame, {});

    Expr *FrameSize =
        buildBuiltinCall(S, Loc, Builtin::BI__builtin_coro_size, {});

    ///////////////////// Make new Call ///////////////////////

    ExprResult NewRef =
        S.BuildDeclRefExpr(OperatorNew, OperatorNew->getType(), VK_LValue, Loc);
    if (NewRef.isInvalid())
      return false;

    ExprResult NewExpr = S.ActOnCallExpr(S.getCurScope(), NewRef.get(), Loc,
                                         FrameSize, Loc, nullptr);
    if (NewExpr.isInvalid())
      return false;

    this->Allocate = NewExpr.get();

    ///////////////////// Make delete Call ///////////////////////

    QualType opDeleteQualType = OperatorDelete->getType();

    ExprResult DeleteRef =
        S.BuildDeclRefExpr(OperatorDelete, opDeleteQualType, VK_LValue, Loc);
    if (DeleteRef.isInvalid())
      return false;

    Expr *CoroFree =
        buildBuiltinCall(S, Loc, Builtin::BI__builtin_coro_free, {FramePtr});

    SmallVector<Expr *, 2> DeleteArgs{CoroFree};

    // Check if we need to pass the size.
    const FunctionProtoType *opDeleteType =
        opDeleteQualType.getTypePtr()->getAs<FunctionProtoType>();
    if (opDeleteType->getNumParams() > 1) {
      DeleteArgs.push_back(FrameSize);
    }

    ExprResult DeleteExpr = S.ActOnCallExpr(S.getCurScope(), DeleteRef.get(),
                                            Loc, DeleteArgs, Loc, nullptr);
    if (DeleteExpr.isInvalid())
      return false;

    this->Deallocate = DeleteExpr.get();

    return true;
  }

  bool makeResultDecl() {
    ExprResult ReturnObject =
      buildPromiseCall(*this, Fn, Loc, "get_return_object", None);
    if (ReturnObject.isInvalid())
      return false;

    RetType = ReturnObject.get()->getType();
    if (RetType->isVoidType()) {
      this->RetDecl = nullptr;
      this->ResultDecl = ReturnObject.get();
      return true;
    }

    if (!RetType->isDependentType()) {
      InitializedEntity Entity =
          InitializedEntity::InitializeResult(Loc, RetType, false);
      ReturnObject = S.PerformMoveOrCopyInitialization(Entity, nullptr, RetType,
                                                       ReturnObject.get());
      if (ReturnObject.isInvalid())
        return false;
    }

    RetDecl = VarDecl::Create(
        S.Context, &FD, FD.getLocation(), FD.getLocation(),
        &S.PP.getIdentifierTable().get("__coro_gro"), RetType,
        S.Context.getTrivialTypeSourceInfo(RetType, Loc), SC_None);

    S.CheckVariableDeclarationType(RetDecl);
    if (RetDecl->isInvalidDecl())
      return false;

    if (RetType == FD.getReturnType()) {
      RetDecl->setNRVOVariable(true);
    }

    S.AddInitializerToDecl(RetDecl, ReturnObject.get(),
                           /*DirectInit=*/false, false); // TypeContainsAuto);

    S.FinalizeDeclaration(RetDecl);

    // Form a declaration statement for the return declaration, so that AST
    // visitors can more easily find it.
    StmtResult ResultStmt =
        S.ActOnDeclStmt(S.ConvertDeclToDeclGroup(RetDecl), Loc, Loc);
    if (ResultStmt.isInvalid())
      return false;

    this->ResultDecl = ResultStmt.get();
    return true;
  }

  bool makeReturnStmt() {
    if (!RetDecl)
      return true;

    ExprResult declRef = S.BuildDeclRefExpr(RetDecl, RetType, VK_LValue, Loc);
    if (declRef.isInvalid())
      return false;
    StmtResult ReturnStmt =
        S.ActOnReturnStmt(Loc, declRef.get(), S.getCurScope());
    if (ReturnStmt.isInvalid())
      return false;
    this->ReturnStmt = ReturnStmt.get();
    return true;
  }

  // Create a static_cast\<T&&>(expr).
  Expr *CastForMoving(Expr *E, QualType T = QualType()) {
    if (T.isNull())
      T = E->getType();
    QualType TargetType = S.BuildReferenceType(
        T, /*SpelledAsLValue*/ false, SourceLocation(), DeclarationName());
    SourceLocation ExprLoc = E->getLocStart();
    TypeSourceInfo *TargetLoc =
        S.Context.getTrivialTypeSourceInfo(TargetType, ExprLoc);

    return S
        .BuildCXXNamedCast(ExprLoc, tok::kw_static_cast, TargetLoc, E,
                           SourceRange(ExprLoc, ExprLoc), E->getSourceRange())
        .get();
  }

  /// \brief Build a variable declaration for move parameter.
  VarDecl *buildVarDecl(SourceLocation Loc, QualType Type, StringRef Name) {
    DeclContext *DC = S.CurContext;
    IdentifierInfo *II = &S.PP.getIdentifierTable().get(Name);
    TypeSourceInfo *TInfo = S.Context.getTrivialTypeSourceInfo(Type, Loc);
    VarDecl *Decl =
        VarDecl::Create(S.Context, DC, Loc, Loc, II, Type, TInfo, SC_None);
    Decl->setImplicit();
    return Decl;
  }

  bool makeParamMoves() {
    for (auto *paramDecl : FD.parameters()) {
      auto Ty = paramDecl->getType();
      if (Ty->isDependentType())
        continue;

      if (auto *RD = Ty->getAsCXXRecordDecl()) {
        if (RD->isUnion())
          continue;
        if (!paramDecl->getIdentifier())
          continue;
        ExprResult ParamRef =
            S.BuildDeclRefExpr(paramDecl, paramDecl->getType(),
                               ExprValueKind::VK_LValue, Loc); // FIXME: scope?
        if (ParamRef.isInvalid())
          return false;

        Expr *RCast = CastForMoving(ParamRef.get());

        SmallString<16> str(paramDecl->getIdentifier()->getName());
        str.append("_copy");
        auto D = buildVarDecl(Loc, Ty, str);

        S.AddInitializerToDecl(D, RCast,
                               /*DirectInit=*/true,
                               /*TypeMayContainAuto=*/false);

        // Convert decl to a statement.
        StmtResult Stmt =
            S.ActOnDeclStmt(S.ConvertDeclToDeclGroup(D), Loc, Loc);
        if (Stmt.isInvalid())
          return false;

        Params.push_back(paramDecl);
        ParamMoves.push_back(Stmt.get());
      }
    }
    return true;
  }
};
}

void Sema::CheckCompletedCoroutineBody(FunctionDecl *FD, Stmt *&Body) {
  FunctionScopeInfo *Fn = getCurFunction();
  assert(Fn && !Fn->CoroutineStmts.empty() && "not a coroutine");

  // Coroutines [stmt.return]p1:
  //   A return statement shall not appear in a coroutine.
  if (Fn->FirstReturnLoc.isValid()) {
    Diag(Fn->FirstReturnLoc, diag::err_return_in_coroutine);
    Stmt *First = Fn->CoroutineStmts[0];
    Diag(First->getLocStart(), diag::note_declared_coroutine_here)
        << (isa<CoawaitExpr>(First) ? 0 : isa<CoyieldExpr>(First) ? 1 : 2);
  }

  SubStmtBuilder Builder(*this, *FD, *Fn, Body);
  if (Builder.isInvalid())
    return FD->setInvalidDecl();

  // Build body for the coroutine wrapper statement.
  Body = CoroutineBodyStmt::Create(
      Context, Builder.Body, Builder.Promise, Builder.InitialSuspend,
      Builder.FinalSuspend, Builder.OnException, Builder.OnFallthrough,
      Builder.Allocate, Builder.Deallocate, Builder.ResultDecl,
      Builder.ReturnStmt, Builder.getParamMoves());
}
