// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines -emit-llvm %s -o - -std=c++14 -O3 | FileCheck %s
#include "Inputs/generator.h"

generator<int> fib(int n) {
  for (int i = 0; i < n; ++i)
    co_yield i;
}

extern "C" void yield(int);

// CHECK-LABEL: @main
int main() {
  for (auto v : fib(5))
    yield(v);
// CHECK: call void @yield(i32 0)
// CHECK: call void @yield(i32 1)
// CHECK: call void @yield(i32 2)
// CHECK: call void @yield(i32 3)
// CHECK: call void @yield(i32 4)
}
