// RUN: %clang_cc1 -std=c++14 -fcoroutines-ts -triple=x86_64-unknown-linux-gnu -emit-llvm -o - %s -fexceptions -fcxx-exceptions -disable-llvm-passes | FileCheck --check-prefix=CHECK-LPAD %s

#include "Inputs/coroutine.h"

struct coro_t {
  void* p;
  ~coro_t();
  struct promise_type {
    coro_t get_return_object();
    coro::suspend_never initial_suspend();
    coro::suspend_never final_suspend();
    void return_void();
    promise_type();
    ~promise_type();
  };
};

struct Cleanup { ~Cleanup(); };
void may_throw();

coro_t f() {
  Cleanup cleanup;
  may_throw();
  co_return;
}
