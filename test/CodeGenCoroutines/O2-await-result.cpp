// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines-ts -emit-llvm %s -o - -std=c++14 -O2 | FileCheck %s
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
  };
};

struct B {
  ~B();
  bool await_ready() { return true; }
  B await_resume() { return {}; }
  template <typename F> void await_suspend(F) {}
};

extern "C" void print(int);

struct A {
  ~A(){}
  bool await_ready() { return true; }
  int await_resume() { return 42; }
  template <typename F> void await_suspend(F) {}
};

extern "C" coro_t f(int n) {
  if (n == 0) {
    print(0);
    co_return;
  }
  int val = co_await A{};
  print(42);
}
// Does not work yet: extern "C" coro g() { B val = co_await B{}; }

// CHECK-LABEL: @main
int main() {
  f(0);
  f(1);
// CHECK:      entry:
// CHECK-NEXT:    call void @print(i32 0)
// CHECK-NEXT:    call void @print(i32 42)
// CHECK-NEXT:    ret i32 0
}
