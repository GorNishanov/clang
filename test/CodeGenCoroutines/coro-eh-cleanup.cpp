// RUN: %clang_cc1 -std=c++14 -fcoroutines-ts -triple=x86_64-pc-windows-msvc18.0.0 -emit-llvm %s -o - -fexceptions -fcxx-exceptions -disable-llvm-passes | FileCheck %s
// RUN: %clang_cc1 -std=c++14 -fcoroutines-ts -triple=x86_64-unknown-linux-gnu -emit-llvm -o - %s -fexceptions -fcxx-exceptions -disable-llvm-passes | FileCheck --check-prefix=CHECK-LPAD %s

#include "Inputs/coroutine.h"

struct coro_t {
  struct promise_type {
    coro_t get_return_object() {
      coro::coroutine_handle<promise_type>{};
      return {};
    }
    coro::suspend_never initial_suspend() { return {}; }
    coro::suspend_never final_suspend() { return {}; }
    void return_void(){}
    void unhandled_exception();
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
// CHECK:       to label %[[CONT:.+]] unwind label %[[EHCLEANUP:.+]]
// CHECK: [[EHCLEANUP]]:
// CHECK:   %[[INNERPAD:.+]] = cleanuppad within none []
// CHECK:   call void @"\01??_DCleanup@@QEAAXXZ"(
// CHECK:   cleanupret from %[[INNERPAD]] unwind label %[[CATCHDISPBB:.+]]
// CHECK: [[CATCHDISPBB]]:
// CHECK: catchswitch within none [label %{{.+}}] unwind label %[[COROENDBB:.+]]

// CHECK: [[COROENDBB]]:
// CHECK-NEXT: %[[CLPAD:.+]] = cleanuppad within none
// CHECK-NEXT: call i1 @llvm.coro.end(i8* null, i1 true) [ "funclet"(token %[[CLPAD]]) ]
// CHECK-NEXT: cleanupret from %[[CLPAD]] unwind label

// CHECK-LPAD: @_Z1fv(
// CHECK-LPAD:   invoke void @_Z9may_throwv()
// CHECK-LPAD:       to label %[[CONT:.+]] unwind label %[[CATCHDISPBB:.+]]
// CHECK-LPAD: [[CATCHDISPBB]]:
// CHECK-LPAD:    landingpad { i8*, i32 }
// CHECK-LPAD-NEXT: catch
// CHECK-LPAD:    call void @_ZN7CleanupD1Ev(
// CHECK-LPAD-NEXT: br label %[[CATCHBB:.+]]

// CHECK-LPAD: [[CATCHBB]]:
// CHECK-LPAD:  invoke void @_ZN6coro_t12promise_type19unhandled_exceptionEv(%"struct.coro_t::promise_type"* %__promise)
// CHECK-LPAD-NEXT: to label %{{.+}} unwind label %[[LPAD:.+]]

// CHECK-LPAD: [[LPAD]]:
// CHECK-LPAD:   invoke void @__cxa_end_catch()
// CHECK-LPAD-NEXT: to label %[[CONT:.+]] unwind label
// CHECK-LPAD: [[CONT]]:
// CHECK-LPAD-NEXT:   br label %[[EHCLEANUP:.+]]

// CHECK-LPAD: [[EHCLEANUP]]:
// CHECK-LPAD:     %[[I1RESUME:.+]] = call i1 @llvm.coro.end(i8* null, i1 true)
// CHECK-LPAD:  br i1  %[[I1RESUME]], label %[[EHRESUME:.+]], label
// CHECK-LPAD: [[EHRESUME]]:
// CHECK-LPAD-NEXT:  %[[EXN:.+]] = load i8*, i8** %exn.slot, align 8
// CHECK-LPAD-NEXT:  %[[SEL:.+]] = load i32, i32* %ehselector.slot, align 4
// CHECK-LPAD-NEXT:  %[[LPAD0:.+]] = insertvalue { i8*, i32 } undef, i8* %[[EXN]], 0
// CHECK-LPAD-NEXT:  %[[LPAD1:.+]] = insertvalue { i8*, i32 } %[[LPAD0]], i32 %[[SEL]], 1
// CHECK-LPAD-NEXT:  resume { i8*, i32 } %[[LPAD1]]
