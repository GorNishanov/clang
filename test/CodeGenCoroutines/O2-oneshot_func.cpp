// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines-ts -emit-llvm %s -o - -std=c++14 -O3 | FileCheck %s
#include "Inputs/coroutine.h"

// This file tests, one shot, movable std::function like thing using coroutine
// for compile-time type erasure and unerasure. At the moment supports only
// functions with no arguments.
template <typename R> struct func {
  struct promise_type {
    R result;
    func get_return_object() { return {this}; }
    coro::suspend_always initial_suspend() { return {}; }
    coro::suspend_always final_suspend() { return {}; }
    void return_value(R v) { result = v; }
  };

  R operator()() {
    h.resume();
    R result = h.promise().result;
    h.destroy();
    h = nullptr;
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

  template <typename F> static func Create(F f) { co_return f(); }

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

extern "C" int yield(int);
extern "C" float fyield(int);

void Do1(func<int> f) { yield(f()); }
void Do2(func<double> f) { yield(f()); }

// CHECK-LABEL: @main(
int main() {
// CHECK: call i32 @yield(i32 43)
// CHECK: call i32 @yield(i32 
  Do1([] { return yield(43); });

// CHECK: call float @fyield(i32 44)
// CHECK: call i32 @yield(i32 
  Do2([] { return fyield(44); });
}
