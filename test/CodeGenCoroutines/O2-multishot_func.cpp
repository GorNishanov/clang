// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines-ts -emit-llvm %s -o - -std=c++14 -O3 | FileCheck %s
#include "Inputs/coroutine.h"

// This file tests, multishot, movable std::function like thing using coroutine
// for compile-time type erasure and unerasure. 
template <typename R> struct func {
  struct Input {R a, b;};

  struct promise_type {
    Input* I;
    R result;
    func get_return_object() { return {this}; }
    coro::suspend_always initial_suspend() { return {}; }
    coro::suspend_never final_suspend() { return {}; }
    template <typename F>
    coro::suspend_always yield_value(F&& f) {
      result = f(I->a, I->b); 
      return {};
    }
  };

  R operator()(Input I) {
    h.promise().I = &I;
    h.resume();
    R result = h.promise().result;
    return result;
  };

  func() {}
  func(func &&rhs) : h(rhs.h) { rhs.h = nullptr; }
  func(func const &) = delete;

  func &operator=(func &&rhs) {
    if (this != &rhs) {
      if (h)
        h.destroy();
      h = rhs.h;
      rhs.h = nullptr;
    }
    return *this;
  }

  template <typename F> static func Create(F f) { 
    for (;;) {
      co_yield f;
    }
  }

  template <typename F> func(F f) : func(Create(f)) {}

  ~func() {
    if (h)
      h.destroy();
  }

private:
  func(promise_type *promise)
      : h(coro::coroutine_handle<promise_type>::from_promise(*promise)) {}
  coro::coroutine_handle<promise_type> h;
};

int Do(int acc, int n, func<int> f) { 
  for (int i = 0; i < n; ++i) 
    acc = f({acc, i});
  return acc;
}

// CHECK: @main
int main() {
  double CaptureMe;
  int result = Do(1, 10, [CaptureMe](int a, int b) {return a + b;});
  return result;
// CHECK:  ret i32 46
}
