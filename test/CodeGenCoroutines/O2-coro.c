// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines-ts -emit-llvm %s -o - -O3 | FileCheck %s
#include "Inputs/coro.h"
void print(int);

void* f() {
  CORO_BEGIN(malloc);

  for (int i = 0;; ++i) {
    print(i);
    CORO_SUSPEND();
  }

  CORO_END(free);
}

// CHECK-LABEL: @main
int main() {
  void* coro = f();
  CORO_RESUME(coro);
  CORO_RESUME(coro);
  CORO_DESTROY(coro);
// CHECK: call void @print(i32 0)
// CHECK: call void @print(i32 1)
// CHECK: call void @print(i32 2)
}
