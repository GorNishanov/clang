// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines -emit-llvm %s -o - -O3 | FileCheck %s

void yield(int);

#define CORO_SUSPEND(IsFinal)                                                  \
  switch (__builtin_coro_suspend(__builtin_coro_save(), IsFinal)) {            \
  case 0:                                                                      \
    if (IsFinal)                                                               \
      __builtin_trap();                                                        \
    break;                                                                     \
  case 1:                                                                      \
    goto coro_Cleanup;                                                         \
  default:                                                                     \
    goto coro_Suspend;                                                         \
  }

#define CORO_BEGIN(AllocFunc)                                                  \
  void *coro_hdl =                                                             \
      __builtin_coro_begin(AllocFunc(__builtin_coro_size(0)), 0, 0, 0, 0);

#define CORO_END(FreeFunc)                                                     \
  coro_Cleanup:                                                                \
  FreeFunc(__builtin_coro_free(coro_hdl));                                     \
  coro_Suspend:                                                                \
  __builtin_coro_end(0);                                                       \
  return coro_hdl;

void free(void *ptr);

void* f() {
  CORO_BEGIN(malloc);

  for (int i = 0;; ++i) {
    yield(i);
    CORO_SUSPEND(0);
  }

  CORO_END(free);
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
