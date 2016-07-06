// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines -emit-llvm %s -o - -std=c++14 -O3 | FileCheck %s
#include "coroutine.h"

struct Data {
  int val = -1;
  int error = 0;
};
struct DataPtr {
  Data *p;
};

struct error {};

struct exp_int {
  int val;
  int error = 0;

  exp_int() {}
  exp_int(int val) : val(val) {}
  exp_int(struct error, int error) : error(error) {}
  exp_int(DataPtr p) : val(p.p->val), error(p.p->error) {}

  struct promise_type {
    Data data;
    DataPtr get_return_object() {
      std::coroutine_handle<promise_type>{};
      return {&data};
    }
    std::suspend_never initial_suspend() { return {}; }
    std::suspend_never final_suspend() { return {}; }
    void return_value(int v) { data.val = v; }
  };

  bool await_ready() { return error == 0; }
  int await_resume() { return val; }
  void await_suspend(std::coroutine_handle<promise_type> h) {
    h.promise().data.error = error;
    h.destroy();
  }

  ~exp_int() {}
};

extern "C" exp_int g() { return {0}; }
extern "C" exp_int h() { return {error{}, 42}; }

extern "C" void yield(int);

extern "C" exp_int f1() {
  yield(11);
  co_await g();
  yield(12);
  co_return 100;
}

extern "C" exp_int f2() {
  yield(21);
  co_await h();
  yield(22);
  co_return 200;
}

// CHECK-LABEL: @main
int main() {
// CHECK: call void @yield(i32 11)
// CHECK: call void @yield(i32 12)
// CHECK: call void @yield(i32 100)
// CHECK: call void @yield(i32 0)
 
  auto c1 = f1();
  yield(c1.val);
  yield(c1.error);

// CHECK: call void @yield(i32 21)
// CHECK: call void @yield(i32 -1)
// CHECK: call void @yield(i32 42)
  auto c2 = f2();
  yield(c2.val);
  yield(c2.error);
}
