// RUN: %clang_cc1 -triple i686-pc-windows-msvc18.0.0 -O3 -fcoroutines-ts -std=c++14 -emit-llvm -o - %s | FileCheck %s
#include "Inputs/coroutine.h"
using namespace std::experimental;

struct minig {
  struct promise_type {
    int current_value;
    suspend_always yield_value(int value) {
      this->current_value = value;
      return {};
    }
    suspend_always initial_suspend() { return {}; }
    suspend_always final_suspend() { return {}; }
    minig get_return_object() { return minig{this}; };
    void return_void() {}
  };

  bool move_next() {
    p.resume();
    return !p.done();
  }
  int current_value() { return p.promise().current_value; }

  minig(minig &&rhs) : p(rhs.p) { rhs.p = nullptr; }

  ~minig() {
    if (p)
      p.destroy();
  }

private:
  explicit minig(promise_type *p)
      : p(coroutine_handle<promise_type>::from_promise(*p)) {}

  coroutine_handle<promise_type> p;
};

minig fib(int n) {
  for (int i = 0; i < n; i++) {
    co_yield i;
  }
}

// CHECK-LABEL: define i32 @main()
int main() {
  int sum = 0;
  auto g = fib(5);
  while (g.move_next()) {
     sum += g.current_value();
  }
  // CHECK: ret i32 10
  return sum;
}
