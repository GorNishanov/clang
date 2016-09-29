// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines-ts -emit-llvm %s -o - -std=c++14 -O3 | FileCheck %s
#include "Inputs/generator.h"

generator<int> fib(int n) {
  for (int i = 0; i < n; ++i)
    co_yield i;
}

extern "C" void print(int);

// CHECK-LABEL: @main
int main() {
  for (auto v : fib(5))
    print(v);
// CHECK: call void @print(i32 0)
// CHECK: call void @print(i32 1)
// CHECK: call void @print(i32 2)
// CHECK: call void @print(i32 3)
// CHECK: call void @print(i32 4)
}
