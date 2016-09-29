// RUN: %clang_cc1 -triple x86_64-pc-windows-msvc18.0.0 -fcoroutines -emit-llvm %s -o - -disable-llvm-passes | FileCheck %s

// CHECK-LABEL: f( 
void f() {
  // CHECK: %0 = call token @llvm.coro.id(i32 32, i8* null, i8* null, i8* null)
  __builtin_coro_id(32, 0, 0, 0);
  // CHECK-NEXT: %1 = call i1 @llvm.coro.alloc(token %0)
  __builtin_coro_alloc();
  // CHECK-NEXT: %2 = call i64 @llvm.coro.size.i64()
  __builtin_coro_size();
  // CHECK-NEXT: %3 = call i8* @llvm.coro.begin(token %0, i8* null)
  __builtin_coro_begin(0);
  // CHECK-NEXT: %4 = call i8* @llvm.coro.frame() 
  __builtin_coro_frame();
  // CHECK-NEXT: call void @llvm.coro.resume(i8* null)
  __builtin_coro_resume(0);
  // CHECK-NEXT: call void @llvm.coro.destroy(i8* null)
  __builtin_coro_destroy(0);
  // CHECK-NEXT: %5 = call i1 @llvm.coro.done(i8* null)
  __builtin_coro_done(0);
  // CHECK-NEXT: %6 = call i8* @llvm.coro.promise(i8* null, i32 32, i1 false)
  __builtin_coro_promise(0, 32, 0);
  // CHECK-NEXT: %7 = call i8* @llvm.coro.free(token %0, i8* null)
  __builtin_coro_free(0);
  // CHECK-NEXT: call void @llvm.coro.end(i8* null, i1 false)
  __builtin_coro_end(0, 0);
  // CHECK-NEXT: %8 = call i8 @llvm.coro.suspend(token none, i1 false)
  __builtin_coro_suspend(0);
  // CHECK-NEXT: %9 = call i1 @llvm.coro.param(i8* null, i8* null)
  __builtin_coro_param(0, 0);
}
