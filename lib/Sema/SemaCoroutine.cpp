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

static ClassTemplateDecl *lookupStdCoroutineHandle(Sema &S, SourceLocation Loc) {
  // FIXME: Cache std::coroutine_handle once we've found it.
  NamespaceDecl *Std = S.lookupStdExperimentalNamespace();
  if (!Std) {
    S.Diag(Loc, diag::err_implied_std_coroutine_handle_not_found);
    return nullptr;
  }

  LookupResult Result(S, &S.PP.getIdentifierTable().get("coroutine_handle"),
    Loc, Sema::LookupOrdinaryName);
  if (!S.LookupQualifiedName(Result, Std)) {
    S.Diag(Loc, diag::err_implied_std_coroutine_handle_not_found);
    return nullptr;
  }
  ClassTemplateDecl *Template = Result.getAsSingle<ClassTemplateDecl>();
  if (!Template) {
    Result.suppressDiagnostics();
    // We found something weird. Complain about the first thing we found.
    NamedDecl *Found = *Result.begin();
    S.Diag(Found->getLocation(), diag::err_malformed_std_coroutine_handle);
    return nullptr;
  }

  // We found some template called std::coroutine_handle. Now verify that it's
  // correct.
  TemplateParameterList *Params = Template->getTemplateParameters();
  if (Params->getMinRequiredArguments() > 1 ||
    !isa<TemplateTypeParmDecl>(Params->getParam(0))) {
    S.Diag(Template->getLocation(), diag::err_malformed_std_coroutine_handle);
    return nullptr;
  }

  return Template;
}

static
QualType buildStdCoroutineHandle(Sema &S, QualType Element, SourceLocation Loc) {
  // FIXME: Cache std::coroutine_handle once we've found it.
  /*
  if (!StdCoroutineHandle) {
    StdCoroutineHandle = lookupStdCoroutineHandle(*this, Loc);
    if (!StdCoroutineHandle)
      return QualType();
  }
  */
  ClassTemplateDecl *StdCoroutineHandle = lookupStdCoroutineHandle(S, Loc);
  if (!StdCoroutineHandle)
    return QualType();

  TemplateArgumentListInfo Args(Loc, Loc);
  Args.addArgument(TemplateArgumentLoc(TemplateArgument(Element),
    S.Context.getTrivialTypeSourceInfo(Element,
      Loc)));
  return S.Context.getCanonicalType(
    S.CheckTemplateIdType(TemplateName(StdCoroutineHandle), Loc, Args));
}

/// Check that this is a context in which a coroutine suspension can appear.
static FunctionScopeInfo *
checkCoroutineContext(Sema &S, SourceLocation Loc, StringRef Keyword) {
  // 'co_await' and 'co_yield' are not permitted in unevaluated operands.
  if (S.isUnevaluatedContext()) {
    S.Diag(Loc, diag::err_coroutine_unevaluated_context) << Keyword;
    return nullptr;
  }

  // Any other usage must be within a function.
  // FIXME: Reject a coroutine with a deduced return type.
  auto *FD = dyn_cast<FunctionDecl>(S.CurContext);
  if (!FD) {
    S.Diag(Loc, isa<ObjCMethodDecl>(S.CurContext)
                    ? diag::err_coroutine_objc_method
                    : diag::err_coroutine_outside_function) << Keyword;
  } else if (isa<CXXConstructorDecl>(FD) || isa<CXXDestructorDecl>(FD)) {
    // Coroutines TS [special]/6:
    //   A special member function shall not be a coroutine.
    //
    // FIXME: We assume that this really means that a coroutine cannot
    //        be a constructor or destructor.
    S.Diag(Loc, diag::err_coroutine_ctor_dtor)
      << isa<CXXDestructorDecl>(FD) << Keyword;
  } else if (FD->isConstexpr()) {
    S.Diag(Loc, diag::err_coroutine_constexpr) << Keyword;
  } else if (FD->isVariadic()) {
    S.Diag(Loc, diag::err_coroutine_varargs) << Keyword;
  } else if (FD->isMain()) {
    S.Diag(FD->getLocStart(), diag::err_coroutine_main);
    S.Diag(Loc, diag::note_declared_coroutine_here)
      << (Keyword == "co_await" ? 0 :
          Keyword == "co_yield" ? 1 : 2);
  } else {
    auto *ScopeInfo = S.getCurFunction();
    assert(ScopeInfo && "missing function scope for function");

    // If we don't have a promise variable, build one now.
    if (!ScopeInfo->CoroutinePromise) {
      QualType T =
          FD->getType()->isDependentType()
              ? S.Context.DependentTy
              : lookupPromiseType(S, FD->getType()->castAs<FunctionProtoType>(),
                                  Loc);
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

  return nullptr;
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
  if (E && !E->getType()->isVoidType()) {
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

namespace {

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

namespace {
struct SubStmtBuilder {
  Stmt *Body = nullptr;
  Stmt *Promise = nullptr;
  Expr *InitialSuspend = nullptr;
  LabelStmt *FinalSuspend = nullptr;
  Stmt *OnException = nullptr;
  Stmt *OnFallthrough = nullptr;
  Expr *Allocate = nullptr;
  LabelStmt *Deallocate = nullptr;
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
    LabelDecl *DestroyLabel =
        LabelDecl::Create(S.Context, S.CurContext, SourceLocation(),
                          S.PP.getIdentifierInfo("coro.destroy.label"));
    LabelDecl *FinalLabel =
      LabelDecl::Create(S.Context, S.CurContext, SourceLocation(),
        S.PP.getIdentifierInfo("coro.final.label"));

    this->Body = Body;
    this->IsValid = makePromiseStmt() && makeInitialSuspend() &&
                    makeFinalSuspend(FinalLabel) && makeOnException() &&
                    makeOnFallthrough() && makeNewAndDeleteExpr(DestroyLabel) &&
                    makeResultDecl() && makeReturnStmt() && makeParamMoves();
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

  bool makeFinalSuspend(LabelDecl *FinalLabel) {
    // Form and check implicit 'co_await p.final_suspend();' statement.
    ExprResult FinalSuspend =
        buildPromiseCall(S, &Fn, Loc, "final_suspend", None);
    // FIXME: Support operator co_await here.
    if (!FinalSuspend.isInvalid())
      FinalSuspend = S.BuildCoawaitExpr(Loc, FinalSuspend.get());
    FinalSuspend = S.ActOnFinishFullExpr(FinalSuspend.get());
    if (FinalSuspend.isInvalid())
      return false;

    // Make it a labeled statement.
    StmtResult Stmt =
        S.ActOnLabelStmt(Loc, FinalLabel, Loc, FinalSuspend.get());

    if (Stmt.isInvalid())
      return false;

    this->FinalSuspend = cast<LabelStmt>(Stmt.get());
    return true;
  }

  // FIXME: Perform analysis of set_exception call.

  bool makeOnException() { return true; }

  // FIXME: add a call to p.return_void().

  bool makeOnFallthrough() { return true; }

  bool makeNewAndDeleteExpr(LabelDecl *label) {
    TypeSourceInfo *TInfo = Fn.CoroutinePromise->getTypeSourceInfo();
    QualType PromiseType = TInfo->getType();

    FunctionDecl *OperatorNew = nullptr;
    FunctionDecl *OperatorDelete = nullptr;

    S.FindAllocationFunctions(Loc, SourceRange(),
                              /*UseGlobal*/ false, PromiseType,
                              /*isArray*/ false, /*PlacementArgs*/ None,
                              OperatorNew, OperatorDelete);

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

    // Make it a labeled statement.
    StmtResult Stmt = S.ActOnLabelStmt(Loc, label, Loc, DeleteExpr.get());

    if (Stmt.isInvalid())
      return false;

    this->Deallocate = cast<LabelStmt>(Stmt.get());

    return true;
  }

  bool makeResultDecl() {
    ExprResult ReturnObject =
        buildPromiseCall(S, &Fn, Loc, "get_return_object", None);
    if (ReturnObject.isInvalid())
      return false;

    RetType = ReturnObject.get()->getType();
    if (RetType == FD.getReturnType()) {
      // GRO is same type as return type of the function
      // don't need to create GRO temporary.
      // Maybe there is a way to make it work efficiently, with NRVO, but I
      // was not able to make it work.
      this->RetDecl = nullptr;
      this->ResultDecl = nullptr;
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
    StmtResult ReturnStmt;
    if (RetDecl) {
      ExprResult declRef = S.BuildDeclRefExpr(RetDecl, RetType, VK_LValue, Loc);
      if (declRef.isInvalid())
        return false;
      ReturnStmt = S.ActOnReturnStmt(Loc, declRef.get(), S.getCurScope());
    } else {
      ExprResult ReturnObject =
          buildPromiseCall(S, &Fn, Loc, "get_return_object", None);
      if (ReturnObject.isInvalid())
        return false;

      RetType = FD.getReturnType();
      if (!RetType->isDependentType()) {
        InitializedEntity Entity =
            InitializedEntity::InitializeResult(Loc, RetType, false);
        ReturnObject = S.PerformMoveOrCopyInitialization(
            Entity, nullptr, RetType, ReturnObject.get());
        if (ReturnObject.isInvalid())
          return false;
      }
      ReturnStmt = S.ActOnReturnStmt(Loc, ReturnObject.get(), S.getCurScope());
    }
    this->ReturnStmt = ReturnStmt.get();

    return !ReturnStmt.isInvalid();
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
