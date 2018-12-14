// RUN: %clang_cc1 -std=c++1z -fcoroutines-ts2 -triple=x86_64-pc-windows-msvc18.0.0 -emit-llvm %s -o - -fexceptions -fcxx-exceptions -disable-llvm-passes | FileCheck %s
// RUN: %clang_cc1 -std=c++1z -fcoroutines-ts2 -triple=x86_64-unknown-linux-gnu -emit-llvm -o - %s -fexceptions -fcxx-exceptions -disable-llvm-passes | FileCheck --check-prefix=CHECK-LPAD %s

namespace std::experimental {
template <typename R, typename... T> struct coroutine_traits {
  using promise_type = typename R::promise_type;
};

template <class Promise = void> struct coroutine_handle;

template <> struct coroutine_handle<void> {
  static coroutine_handle from_address(void *) noexcept;
  coroutine_handle() = default;
  void *address() noexcept;
  template <class PromiseType>
  coroutine_handle(coroutine_handle<PromiseType>) noexcept;
};
template <class Promise> struct coroutine_handle: coroutine_handle<void> {
  coroutine_handle() = default;
  static coroutine_handle from_address(void *) noexcept;
};
}

struct suspend_always {
  bool await_ready() noexcept;
  void await_suspend(std::experimental::coroutine_handle<>) noexcept;
  void await_resume() noexcept;
};

struct coro_t {
  struct promise_type {
    coro_t get_return_object() noexcept;
    std::experimental::coroutine_handle<> final_suspend() noexcept;
    void return_void() noexcept;
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

// CHECK: @"?f@@YA?AUcoro_t@@XZ"(
// CHECK:   invoke void @"?may_throw@@YAXXZ"()
// CHECK:       to label %[[CONT:.+]] unwind label %[[EHCLEANUP:.+]]
// CHECK: [[EHCLEANUP]]:
// CHECK:   %[[INNERPAD:.+]] = cleanuppad within none []
// CHECK:   call void @"??1Cleanup@@QEAA@XZ"(
// CHECK:   cleanupret from %{{.+}} unwind label %[[COROSAVEBB:.+]]

// CHECK: [[COROSAVEBB]]:
// CHECK-NEXT: cleanuppad within none []
// CHECK-NEXT: %[[COROSAVETOK:.+]] = call token @llvm.coro.save(
// CHECK-NEXT: cleanupret from %{{.+}} unwind label %[[CATCHDISPATCH:.+]]

// CHECK: [[CATCHDISPATCH]]:
// CHECK:   catchswitch within none [label %[[CATCHPAD:.+]]] unwind label %[[COROEHBB:.+]]
// CHECK: [[CATCHPAD]]:
// CHECK:   invoke void @"?unhandled_exception@promise_type@coro_t@@QEAAXXZ"

// CHECK: [[COROEHBB]]:
// CHECK-NEXT: cleanuppad within none []
// CHECK-NEXT: call i1 @llvm.coro.eh.suspend(token %[[COROSAVETOK]])
// CHECK-NEXT: cleanupret from %{{.+}} unwind label %[[COROENDBB:.+]]

// CHECK: [[COROENDBB]]:
// CHECK-NEXT: %[[CLPAD:.+]] = cleanuppad within none
// CHECK-NEXT: call i1 @llvm.coro.end(i8* null, i1 true) [ "funclet"(token %[[CLPAD]]) ]
// CHECK-NEXT: cleanupret from %[[CLPAD]] unwind label

// CHECK-LPAD: @_Z1fv(
// CHECK-LPAD:   invoke void @_Z9may_throwv()
// CHECK-LPAD:       to label %[[CONT:.+]] unwind label %[[EHCLEANUP:.+]]
// CHECK-LPAD: [[EHCLEANUP]]:
// CHECK-LPAD:    landingpad { i8*, i32 }
// CHECK-LPAD:          catch
// CHECK-LPAD:   call void @_ZN7CleanupD1Ev(
// CHECK-LPAD-NEXT:   call token @llvm.coro.save(

// CHECK-LPAD:   call i8* @__cxa_begin_catch
// CHECK-LPAD:   invoke void @_ZN6coro_t12promise_type19unhandled_exceptionEv
// CHECK-LPAD:      to label %{{.+}} unwind label %[[ENDCATCHBB:.+]]

// CHECK-LPAD:  [[ENDCATCHBB]]:
// CHECK-LPAD:   invoke void @__cxa_end_catch()
// CHECK-LPAD:             to label %[[ENDCATCHCLEANUP:.+]] unwind label

// CHECK-LPAD: [[ENDCATCHCLEANUP]]:
// CHECK-LPAD-NEXT: br label %[[LPADCLEANUP:.+]]

// CHECK-LPAD: [[LPADCLEANUP]]:
// CHECK-LPAD-NEXT:  %[[COND1:.+]] = call i1 @llvm.coro.eh.suspend(token
// CHECK-LPAD-NEXT:  br i1 %[[COND1]], label %[[EHRES:.+]], label %[[CONT:.+]]

// CHECK-LPAD: [[CONT]]:
// CHECK-LPAD-NEXT:  %[[COND2:.+]] = call i1 @llvm.coro.end(i8* null, i1 true)
// CHECK-LPAD-NEXT:  br i1 %[[COND2]], label %[[EHRES]], label %{{.+}}

// CHECK-LPAD: [[EHRES]]:
// CHECK-LPAD-NEXT:  %[[exn:.+]] = load i8*, i8** %exn.slot, align 8
// CHECK-LPAD-NEXT:  %[[sel:.+]] = load i32, i32* %ehselector.slot, align 4
// CHECK-LPAD-NEXT:  %[[val1:.+]] = insertvalue { i8*, i32 } undef, i8* %[[exn]], 0
// CHECK-LPAD-NEXT:  %[[val2:.+]] = insertvalue { i8*, i32 } %[[val1]], i32 %[[sel]], 1
// CHECK-LPAD-NEXT:  resume { i8*, i32 } %[[val2]]
