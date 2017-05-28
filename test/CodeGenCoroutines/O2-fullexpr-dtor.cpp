// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines-ts -emit-llvm %s -o - -std=c++14 -O2 | FileCheck %s
#include "Inputs/coroutine.h"

using namespace std::experimental;

extern "C" void ctor();
extern "C" void dtor();
extern "C" void stuff();

struct Noisy {
  Noisy() { ctor(); }
  ~Noisy() { dtor(); }
};

struct Bug {
  bool await_ready() { return true; }
  void await_suspend(std::experimental::coroutine_handle<>) {}
  Noisy await_resume() { return {}; }
};
struct coro2 {
  struct promise_type {
    suspend_never initial_suspend() { return{}; }
    suspend_never final_suspend() { return{}; }
    coro2 get_return_object() { return{}; }
    void return_void() {}
    Bug yield_value(int) { return {}; }
  };
};

// Checks that destructors are correctly invoked for the object returned by
// coawait.
// CHECK-LABEL: @a(
extern "C" coro2 a() {
  auto x = co_await Bug{};
  stuff();
// CHECK: call void @ctor()
// CHECK: call void @stuff()
// CHECK: call void @dtor()
}
// CHECK-LABEL: @b(
extern "C" coro2 b() {
  co_await Bug{};
  stuff();
// CHECK: call void @ctor()
// CHECK: call void @dtor()
// CHECK: call void @stuff()
}
// CHECK-LABEL: @c(
extern "C" coro2 c() {
  auto x = co_yield 42;
  stuff();
// CHECK: call void @ctor()
// CHECK: call void @stuff()
// CHECK: call void @dtor()
}
// CHECK-LABEL: @d(
extern "C" coro2 d() {
  co_yield 42;
  stuff();
// CHECK: call void @ctor()
// CHECK: call void @dtor()
// CHECK: call void @stuff()
}
