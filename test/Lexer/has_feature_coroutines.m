// RUN: %clang_cc1 -E -fcoroutines-ts %s -o - | FileCheck --check-prefix=CHECK-HAS-COROUTINES %s
// RUN: %clang_cc1 -E %s -o - | FileCheck --check-prefix=CHECK-NO-COROUTINES %s
// RUN: %clang_cc1 -E -x c -fcoroutines-ts %s -o - | FileCheck --check-prefix=CHECK-HAS-COROUTINES %s

#if __has_feature(coroutines)
int has_coroutines();
#else
int no_coroutines();
#endif

// CHECK-HAS-COROUTINES: has_coroutines
// CHECK-NO-COROUTINES: no_coroutines

