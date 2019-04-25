// RUN: %clang_cc1 -std=c++1z -fcoroutines-ts -triple=x86_64-pc-windows-msvc18.0.0 -emit-llvm %s -o - -fexceptions -fcxx-exceptions -disable-llvm-passes | FileCheck %s
// RUN: %clang_cc1 -std=c++1z -fcoroutines-ts -triple=x86_64-unknown-linux-gnu -emit-llvm -o - %s -fexceptions -fcxx-exceptions -disable-llvm-passes | FileCheck --check-prefix=CHECK-LPAD %s

namespace std::experimental {
template <typename R, typename... T> struct coroutine_traits {
  using promise_type = typename R::promise_type;
};

template <class Promise = void> struct coroutine_handle;

template <> struct coroutine_handle<void> {
  static coroutine_handle from_address(void *) noexcept;
  coroutine_handle() = default;
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
    suspend_always initial_suspend() noexcept;
    suspend_always final_suspend() noexcept;
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
// CHECK-NEXT: cleanuppad within none
// CHECK-NEXT: %[[COROSAVE:.+]] = call token @llvm.coro.save(i8*
// CHECK-NEXT: cleanupret from %{{.+}} unwind label %[[CATCHDISPATCH:.+]]

// CHECK: [[CATCHDISPATCH]]:
// CHECK-NEXT: catchswitch within none [label %[[CATCHPAD:.+]]] unwind label %[[COROEHSUS:.+]]

// CHECK: [[CATCHPAD]]:
// CHECK-NEXT: catchpad within
// CHECK-NEXT: invoke void @"?unhandled_exception@promise_type@coro_t@@QEAAXXZ"
// CHECK-NEXT:    to label %{{.+}} unwind label %[[COROEHSUS:.+]]

// CHECK: [[COROEHSUS]]:
// CHECK-NEXT: cleanuppad within
// CHECK-NEXT: call void @llvm.coro.eh.suspend(token %[[COROSAVE]])
// CHECK-NEXT: cleanupret from %{{.+}} unwind label %[[COROFREEBB:.+]]

// CHECK: [[COROFREEBB]]:
// CHECK-NEXT: cleanuppad within
// CHECK-NEXT: call i8* @llvm.coro.free(
// CHECK: cleanupret from %{{.+}} unwind label %[[COROENDBB:.+]]

// CHECK: [[COROENDBB]]:
// CHECK-NEXT: %[[CLPAD:.+]] = cleanuppad within none
// CHECK-NEXT: call i1 @llvm.coro.end(i8* null, i1 true) [ "funclet"(token %[[CLPAD]]) ]
// CHECK-NEXT: cleanupret from %[[CLPAD]] unwind to caller

// CHECK-LPAD: @_Z1fv(
// CHECK-LPAD:   invoke void @_Z9may_throwv()
// CHECK-LPAD:       to label %[[CONT:.+]] unwind label %[[EHCLEANUP:.+]]
// CHECK-LPAD: [[EHCLEANUP]]:
// CHECK-LPAD:    landingpad { i8*, i32 }
// CHECK-LPAD:          catch
// CHECK-LPAD:   call void @_ZN7CleanupD1Ev(
// CHECK-LPAD:   %[[LPCOROSAVE:.+]] = call token @llvm.coro.save(i8*
// CHECK-LPAD:   call i8* @__cxa_begin_catch
// CHECK-LPAD:   invoke void @_ZN6coro_t12promise_type19unhandled_exceptionEv
// CHECK-LPAD-NEXT:  to label %{{.+}} unwind label

// CHECK-LPAD: call void @llvm.coro.eh.suspend(token %[[LPCOROSAVE]])
