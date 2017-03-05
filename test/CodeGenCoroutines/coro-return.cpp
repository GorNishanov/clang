// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines-ts -std=c++14 -emit-llvm %s -o - -disable-llvm-passes | FileCheck %s
#include "Inputs/coroutine.h"

template<>
struct std::experimental::coroutine_traits<void> {
  struct promise_type {
    void get_return_object();
    coro::suspend_always initial_suspend();
    coro::suspend_always final_suspend();
    void return_void();
  };
};

// CHECK-LABEL: f0(
extern "C" void f0() {
  // CHECK: %__promise = alloca %"struct.std::experimental::coroutines_v1::coroutine_traits<void>::promise_type"
  // CHECK: %call = call i8* @_Znwm(
  // CHECK: call void @_ZNSt12experimental13coroutines_v116coroutine_traitsIvJEE12promise_type11return_voidEv(%"struct.std::experimental::coroutines_v1::coroutine_traits<void>::promise_type"* %__promise)
  // CHECK: call void @_ZdlPv
  co_return;
}

template<>
struct std::experimental::coroutine_traits<int> {
  struct promise_type {
    int get_return_object();
    suspend_always initial_suspend();
    suspend_always final_suspend();
    void return_value(int);
  };
};

// CHECK-LABEL: f1(
extern "C" int f1() {
  // CHECK: %__promise = alloca %"struct.std::experimental::coroutines_v1::coroutine_traits<int>::promise_type"
  // CHECK: %call = call i8* @_Znwm(
  // CHECK: call void @_ZNSt12experimental13coroutines_v116coroutine_traitsIiJEE12promise_type12return_valueEi(%"struct.std::experimental::coroutines_v1::coroutine_traits<int>::promise_type"* %__promise, i32 42)
  // CHECK: call void @_ZdlPv
  co_return 42;
}
