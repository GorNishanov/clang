// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines -emit-llvm %s -o - -std=c++14 -O3 | FileCheck %s
#include "Inputs/coroutine.h"

struct coro {
  struct promise_type {
    coro get_return_object() {
      std::coroutine_handle<promise_type>{};
      return {};
    }
    std::suspend_never initial_suspend() { return {}; }
    std::suspend_never final_suspend() { return {}; }
  };
};

struct B {
  ~B();
  bool await_ready() { return true; }
  B await_resume() { return {}; }
  template <typename F> void await_suspend(F) {}
};

struct A {
  ~A(){}
  bool await_ready() { return true; }
  int await_resume() { return 42; }
  template <typename F> void await_suspend(F) {}

};

extern "C" coro f() { int val = co_await A{}; }
// Does not work yet: extern "C" coro g() { B val = co_await B{}; }

// CHECK-LABEL: @main
int main() {
  f();
// CHECK:      entry:
// CHECK-NEXT:    ret i32 0
}
