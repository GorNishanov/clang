// RUN: %clang_cc1 -std=c++14 -fcoroutines-ts -triple=x86_64-pc-windows-msvc18.0.0 -emit-llvm %s -o - -fexceptions -fcxx-exceptions -disable-llvm-passes | FileCheck %s
// RUN: %clang_cc1 -std=c++14 -fcoroutines-ts -triple=x86_64-unknown-linux-gnu -emit-llvm -o - %s -fexceptions -fcxx-exceptions -disable-llvm-passes | FileCheck --check-prefix=CHECK-LPAD %s

#include "Inputs/coroutine.h"

namespace std {
  using exception_ptr = int;
  exception_ptr current_exception();
}

struct coro_t {
  struct promise_type {
    coro_t get_return_object() {
      coro::coroutine_handle<promise_type>{};
      return {};
    }
    coro::suspend_never initial_suspend() { return {}; }
    coro::suspend_never final_suspend() { return {}; }
    void return_void(){}
    void set_exception(std::exception_ptr &&) noexcept; // {}
  };
};

struct Cleanup { ~Cleanup(); };
void may_throw();

coro_t f() {
  Cleanup x;
  may_throw();
  co_return;
}

// CHECK: @"\01?f@@YA?AUcoro_t@@XZ"(
// CHECK:   invoke void @"\01?may_throw@@YAXXZ"()
// CHECK:       to label %{{.+}} unwind label %[[EHCLEANUP:.+]]
// CHECK: [[EHCLEANUP]]:
// CHECK:   %[[INNERPAD:.+]] = cleanuppad within none []
// CHECK:   call void @"\01??_DCleanup@@QEAA@XZ"(
// CHECK:   cleanupret from %[[INNERPAD]] unwind label %[[CATCHSW:.+]]
// CHECK: [[CATCHSW]]:
// CHECK:   %[[CATCHSWTOK:.+]] = catchswitch within none [label %[[CATCH:.+]]] unwind label
// CHECK: [[CATCH]]:
// CHECK:   catchpad within [[CATCHSWTOK:.+]]
// CHECK: %[[TOK:.+]] = invoke i32 @"\01?current_exception@std@@YAHXZ"()
// CHECK:   to label %{{.+}} unwind label %[[COROENDBB:.+]]
// CHECK: [[COROENDBB]]:
// CHECK-NEXT: %[[CLPAD:.+]] = cleanuppad within none
// CHECK-NEXT: call i1 @llvm.coro.end(i8* null, i1 true) [ "funclet"(token %[[CLPAD]]) ]
// CHECK-NEXT: cleanupret from %[[CLPAD]] unwind label 

// CHECK-LPAD: @_Z1fv(
// CHECK-LPAD:   invoke void @_Z9may_throwv()
// CHECK-LPAD:       to label %[[CONT:.+]] unwind label %[[CATCHBB:.+]]
// CHECK-LPAD: [[CATCHBB]]:
// CHECK-LPAD:    landingpad { i8*, i32 }
// CHECK-LPAD:          catch i8* null
// CHECK-LPAD:    call i8* @__cxa_begin_catch
// CHECK-LPAD:    %[[EPTR:.+]] = invoke i32 @_ZSt17current_exceptionv()
// CHECK-LPAD:          to label %[[SETE:.+]] unwind label
// CHECK-LPAD: [[SETE]]:
// CHECK-LPAD:    store i32 %[[EPTR]], i32* %[[TMP:.+]], align 4
// CHECK-LPAD:    call void @_ZN6coro_t12promise_type13set_exceptionEOi(%"struct.coro_t::promise_type"* %__promise, i32* dereferenceable(4) %[[TMP]])
// CHECK-LPAD:    invoke void @__cxa_end_catch()
