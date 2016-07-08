// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines -emit-llvm %s -o - -O3 | FileCheck %s

void yield(int);

#define SUSPEND(IsFinal)                                                       \
  switch (__builtin_coro_suspend(__builtin_coro_save(), IsFinal)) {            \
  case 0:                                                                      \
    if (IsFinal)                                                               \
      __builtin_trap();                                                        \
    break;                                                                     \
  case 1:                                                                      \
    goto Cleanup;                                                              \
  default:                                                                     \
    goto Suspend;                                                              \
  }

void* f() {
  void* ca = __builtin_coro_alloc();
  void* mem = ca;
  if (!mem) mem = malloc(__builtin_coro_size(__builtin_coro_frame()));

  void* hdl = __builtin_coro_begin(mem,ca,0,0,0);

  for (int i = 0;; ++i) {
    yield(i);
    SUSPEND(0);
  }

Cleanup:
  {
    void* mem_to_free = __builtin_coro_free(__builtin_coro_frame());
    if (mem_to_free) free(mem_to_free);
  }
Suspend:  
  __builtin_coro_end(0);
  return hdl;
}

// CHECK-LABEL: @main
int main() {
  void* coro = f();
  __builtin_coro_resume(coro);
  __builtin_coro_resume(coro);
  __builtin_coro_destroy(coro);
// CHECK: call void @yield(i32 0)
// CHECK: call void @yield(i32 1)
// CHECK: call void @yield(i32 2)
}
