// RUN: %clang -### %s 2>&1 | FileCheck -check-prefix=CHECK-NO-CORO %s
// RUN: %clang -fcoroutines -### %s 2>&1 | FileCheck -check-prefix=CHECK-NO-CORO %s
// RUN: %clang -fno-coroutines -### %s 2>&1 | FileCheck -check-prefix=CHECK-NO-CORO %s
// RUN: %clang -fno-coroutines -fcoroutines -### %s 2>&1 | FileCheck -check-prefix=CHECK-NO-CORO %s
// CHECK-NO-CORO-NOT: -fcoroutines

