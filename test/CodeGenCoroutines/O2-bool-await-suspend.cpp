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

struct NoSuspend {
  bool await_ready() { return false; }
  void await_resume() {}
  template <typename F> bool await_suspend(F) { return false; }
};

extern "C" void print(int);

struct DoSuspend {
  bool await_ready() { return false; }
  void await_resume() {}
  template <typename F> bool await_suspend(F) { return true; }
};

extern "C" coro_t f() {
  print(1);
  co_await DoSuspend{};
  print(2);
}

extern "C" coro_t g() {
  print(3);
  co_await NoSuspend{};
  print(4);
}

// CHECK-LABEL: @main(
int main() {
  f();
  g();
// CHECK:    call void @print(i32 1)
// CHECK:    call void @print(i32 3)
// CHECK:    call void @print(i32 4)
// CHECK:    ret i32 0
}
