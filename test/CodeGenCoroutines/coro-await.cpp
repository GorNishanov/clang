// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines-ts -std=c++14 -emit-llvm %s -o - -disable-llvm-passes | FileCheck %s

namespace std {
namespace experimental {
template <typename... T>
struct coroutine_traits;

template <typename Promise = void> struct coroutine_handle;

template <>
struct coroutine_handle<void> {
  void *ptr;
  static coroutine_handle from_address(void *);
};

template <typename Promise>
struct coroutine_handle : coroutine_handle<> {
  static coroutine_handle from_address(void *);
};

}
}

struct suspend_always {
  int stuff;
  bool await_ready();
  void await_suspend(std::experimental::coroutine_handle<>);
  void await_resume();
};

template<>
struct std::experimental::coroutine_traits<void> {
  struct promise_type {
    void get_return_object();
    suspend_always initial_suspend();
    suspend_always final_suspend();
    void return_void();
  };
};

// CHECK-LABEL: f0(
extern "C" void f0() {

  co_await suspend_always{};
  // See if we need to suspend:
  // --------------------------
  // CHECK: %[[READY:.+]] = call zeroext i1 @_ZN14suspend_always11await_readyEv(%struct.suspend_always* %[[AWAITABLE:.+]])
  // CHECK: br i1 %[[READY]], label %[[READY_BB:.+]], label %[[SUSPEND_BB:.+]]

  // If we are suspending:
  // ---------------------
  // CHECK: [[SUSPEND_BB]]:
  // CHECK: %[[SUSPEND_ID:.+]] = call token @llvm.coro.save(
  // ---------------------------
  // Build the coroutine handle and pass it to await_suspend
  // ---------------------------
  // CHECK: %[[FRAME:.+]] = call i8* @llvm.coro.frame()
  // CHECK: call i8* @_ZNSt12experimental16coroutine_handleINS_16coroutine_traitsIJvEE12promise_typeEE12from_addressEPv(i8* %[[FRAME]])
  //   ... many lines of code to coerce coroutine_handle into an i8* scalar
  // CHECK: %[[CH:.+]] = load i8*, i8** %{{.+}}
  // CHECK: call void @_ZN14suspend_always13await_suspendENSt12experimental16coroutine_handleIvEE(%struct.suspend_always* %[[AWAITABLE]], i8* %[[CH]])
  // -------------------------
  // Generate a suspend point:
  // -------------------------
  // CHECK: %[[OUTCOME:.+]] = call i8 @llvm.coro.suspend(token %[[SUSPEND_ID]], i1 false)
  // CHECK: switch i8 %[[OUTCOME]], label %[[RET_BB:.+]] [
  // CHECK:   i8 0, label %[[READY_BB]]
  // CHECK:   i8 1, label %[[CLEANUP_BB:.+]]
  // CHECK: ]

  // Cleanup code goes here:
  // -----------------------
  // CHECK: [[CLEANUP_BB]]:
  
  // When coroutine is resumed, call await_resume
  // --------------------------
  // CHECK: [[READY_BB]]:
  // CHECK:  call void @_ZN14suspend_always12await_resumeEv(%struct.suspend_always* %[[AWAITABLE]])
}

struct suspend_maybe {
  float stuff;
  ~suspend_maybe();
  bool await_ready();
  bool await_suspend(std::experimental::coroutine_handle<>);
  void await_resume();
};


template<>
struct std::experimental::coroutine_traits<void,int> {
  struct promise_type {
    void get_return_object();
    suspend_always initial_suspend();
    suspend_always final_suspend();
    void return_void();
    suspend_maybe yield_value(int);
  };
};

// CHECK-LABEL: f1(
extern "C" void f1(int) {
  co_yield 42;
  // CHECK: %[[PROMISE:.+]] = alloca %"struct.std::experimental::coroutine_traits<void, int>::promise_type"
  // CHECK: call void @_ZNSt12experimental16coroutine_traitsIJviEE12promise_type11yield_valueEi(%struct.suspend_maybe* sret %[[AWAITER:.+]], %"struct.std::experimental::coroutine_traits<void, int>::promise_type"* %[[PROMISE]], i32 42)

  // See if we need to suspend:
  // --------------------------
  // CHECK: %[[READY:.+]] = call zeroext i1 @_ZN13suspend_maybe11await_readyEv(%struct.suspend_maybe* %[[AWAITABLE]])
  // CHECK: br i1 %[[READY]], label %[[READY_BB:.+]], label %[[SUSPEND_BB:.+]]

  // If we are suspending:
  // ---------------------
  // CHECK: [[SUSPEND_BB]]:
  // CHECK: %[[SUSPEND_ID:.+]] = call token @llvm.coro.save(
  // ---------------------------
  // Build the coroutine handle and pass it to await_suspend
  // ---------------------------
  // CHECK: %[[FRAME:.+]] = call i8* @llvm.coro.frame()
  // CHECK: call i8* @_ZNSt12experimental16coroutine_handleINS_16coroutine_traitsIJviEE12promise_typeEE12from_addressEPv(i8* %[[FRAME]])
  //   ... many lines of code to coerce coroutine_handle into an i8* scalar
  // CHECK: %[[CH:.+]] = load i8*, i8** %{{.+}}
  // CHECK: %[[YES:.+]] = call zeroext i1 @_ZN13suspend_maybe13await_suspendENSt12experimental16coroutine_handleIvEE(%struct.suspend_maybe* %[[AWAITABLE]], i8* %[[CH]])
  // -------------------------------------------
  // See if await_suspend decided not to suspend
  // -------------------------------------------
  // CHECK: br i1 %[[YES]], label %[[SUSPEND_PLEASE:.+]], label %[[READY_BB]]

  // CHECK: [[SUSPEND_PLEASE]]:
  // CHECK:    call i8 @llvm.coro.suspend(token %[[SUSPEND_ID]], i1 false)

  // CHECK: [[READY_BB]]:
  // CHECK:     call void @_ZN13suspend_maybe12await_resumeEv(%struct.suspend_maybe* %[[AWAITABLE]])
}
