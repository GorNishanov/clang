// RUN: not %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines -emit-llvm %s -o - -std=c++14 -O3 2>&1 | FileCheck %s
#include "coroutine.h"

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

struct A {
  ~A(){}
  bool await_ready() { return true; }
  int await_resume() { return 42; }
  template <typename F> void await_suspend(F) {}

};

// CHECK: Instruction does not dominate all uses!
//   %call12 = call i32 @"\01?await_resume@A@@QEAAHXZ"(%struct.A* %ref.tmp6)
//   store i32 %call12, i32* %val, align 4, !tbaa !7
extern "C" coro f() {
#if 1 // disable this and it will compile
  int val = 
#endif
    co_await A{}; // results in broken IR due to cleanup branch
                  // bypassing the return value of the expression
}

int main() {
  f();
}
