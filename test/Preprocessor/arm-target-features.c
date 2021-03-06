// RUN: %clang -target armv8a-none-linux-gnu -x c -E -dM %s -o - | FileCheck --check-prefix=CHECK-V8A %s
// CHECK-V8A: __ARMEL__ 1
// CHECK-V8A: __ARM_ARCH 8
// CHECK-V8A: __ARM_ARCH_8A__ 1
// CHECK-V8A: __ARM_FEATURE_CRC32 1
// CHECK-V8A: __ARM_FEATURE_DIRECTED_ROUNDING 1
// CHECK-V8A: __ARM_FEATURE_NUMERIC_MAXMIN 1
// CHECK-V8A: __ARM_FP 0xE
// CHECK-V8A: __ARM_FP16_ARGS 1
// CHECK-V8A: __ARM_FP16_FORMAT_IEEE 1

// RUN: %clang -target armv7a-none-linux-gnu -x c -E -dM %s -o - | FileCheck --check-prefix=CHECK-V7 %s
// CHECK-V7: __ARMEL__ 1
// CHECK-V7: __ARM_ARCH 7
// CHECK-V7: __ARM_ARCH_7A__ 1
// CHECK-V7-NOT: __ARM_FEATURE_CRC32
// CHECK-V7-NOT: __ARM_FEATURE_NUMERIC_MAXMIN
// CHECK-V7-NOT: __ARM_FEATURE_DIRECTED_ROUNDING
// CHECK-V7: __ARM_FP 0xC

// RUN: %clang -target x86_64-apple-macosx10.10 -arch armv7s -x c -E -dM %s -o - | FileCheck --check-prefix=CHECK-V7S %s
// CHECK-V7S: __ARMEL__ 1
// CHECK-V7S: __ARM_ARCH 7
// CHECK-V7S: __ARM_ARCH_7S__ 1
// CHECK-V7S-NOT: __ARM_FEATURE_CRC32
// CHECK-V7S-NOT: __ARM_FEATURE_NUMERIC_MAXMIN
// CHECK-V7S-NOT: __ARM_FEATURE_DIRECTED_ROUNDING
// CHECK-V7S: __ARM_FP 0xE

// RUN: %clang -target armv8a -mfloat-abi=hard -x c -E -dM %s | FileCheck --check-prefix=CHECK-V8-BAREHF %s
// CHECK-V8-BAREHF: __ARMEL__ 1
// CHECK-V8-BAREHF: __ARM_ARCH 8
// CHECK-V8-BAREHF: __ARM_ARCH_8A__ 1
// CHECK-V8-BAREHF: __ARM_FEATURE_CRC32 1
// CHECK-V8-BAREHF: __ARM_FEATURE_DIRECTED_ROUNDING 1
// CHECK-V8-BAREHF: __ARM_FEATURE_NUMERIC_MAXMIN 1
// CHECK-V8-BAREHP: __ARM_FP 0xE
// CHECK-V8-BAREHF: __ARM_NEON__ 1
// CHECK-V8-BAREHF: __ARM_PCS_VFP 1
// CHECK-V8-BAREHF: __VFP_FP__ 1

// RUN: %clang -target armv8a -mfloat-abi=hard -mfpu=fp-armv8 -x c -E -dM %s | FileCheck --check-prefix=CHECK-V8-BAREHF-FP %s
// CHECK-V8-BAREHF-FP-NOT: __ARM_NEON__ 1
// CHECK-V8-BAREHP-FP: __ARM_FP 0xE
// CHECK-V8-BAREHF-FP: __VFP_FP__ 1

// RUN: %clang -target armv8a -mfloat-abi=hard -mfpu=neon-fp-armv8 -x c -E -dM %s | FileCheck --check-prefix=CHECK-V8-BAREHF-NEON-FP %s
// RUN: %clang -target armv8a -mfloat-abi=hard -mfpu=crypto-neon-fp-armv8 -x c -E -dM %s | FileCheck --check-prefix=CHECK-V8-BAREHF-NEON-FP %s
// CHECK-V8-BAREHP-NEON-FP: __ARM_FP 0xE
// CHECK-V8-BAREHF-NEON-FP: __ARM_NEON__ 1
// CHECK-V8-BAREHF-NEON-FP: __VFP_FP__ 1

// RUN: %clang -target armv8a -mnocrc -x c -E -dM %s | FileCheck --check-prefix=CHECK-V8-NOCRC %s
// CHECK-V8-NOCRC-NOT: __ARM_FEATURE_CRC32 1

// Check that -mhwdiv works properly for armv8/thumbv8 (enabled by default).

// RUN: %clang -target armv8 -x c -E -dM %s -o - | FileCheck --check-prefix=V8 %s
// RUN: %clang -target armv8 -mthumb -x c -E -dM %s -o - | FileCheck --check-prefix=V8 %s
// RUN: %clang -target armv8-eabi -x c -E -dM %s -o - | FileCheck --check-prefix=V8 %s
// RUN: %clang -target armv8-eabi -mthumb -x c -E -dM %s -o - | FileCheck --check-prefix=V8 %s
// V8:#define __ARM_ARCH_EXT_IDIV__ 1

// RUN: %clang -target armv8 -mhwdiv=none -x c -E -dM %s -o - | FileCheck --check-prefix=NOHWDIV-V8 %s
// RUN: %clang -target armv8 -mthumb -mhwdiv=none -x c -E -dM %s -o - | FileCheck --check-prefix=NOHWDIV-V8 %s
// RUN: %clang -target armv8 -mhwdiv=thumb -x c -E -dM %s -o - | FileCheck --check-prefix=NOHWDIV-V8 %s
// RUN: %clang -target armv8 -mthumb -mhwdiv=arm -x c -E -dM %s -o - | FileCheck --check-prefix=NOHWDIV-V8 %s
// NOHWDIV-V8-NOT:#define __ARM_ARCH_EXT_IDIV__

// RUN: %clang -target armv8a -x c -E -dM %s -o - | FileCheck --check-prefix=V8A %s
// RUN: %clang -target armv8a -mthumb -x c -E -dM %s -o - | FileCheck --check-prefix=V8A %s
// RUN: %clang -target armv8a-eabi -x c -E -dM %s -o - | FileCheck --check-prefix=V8A %s
// RUN: %clang -target armv8a-eabi -x c -E -dM %s -o - | FileCheck --check-prefix=V8A %s
// V8A:#define __ARM_ARCH_EXT_IDIV__ 1
// V8A:#define __ARM_FP 0xE

// RUN: %clang -target armv8m.base-none-linux-gnu -x c -E -dM %s -o - | FileCheck --check-prefix=V8M_BASELINE %s
// V8M_BASELINE: __ARM_ARCH 8
// V8M_BASELINE: __ARM_ARCH_8M_BASE__ 1
// V8M_BASELINE: __ARM_ARCH_EXT_IDIV__ 1
// V8M_BASELINE-NOT: __ARM_ARCH_ISA_ARM
// V8M_BASELINE: __ARM_ARCH_ISA_THUMB 1
// V8M_BASELINE: __ARM_ARCH_PROFILE 'M'
// V8M_BASELINE-NOT: __ARM_FEATURE_CRC32
// V8M_BASELINE-NOT: __ARM_FEATURE_DSP
// V8M_BASELINE-NOT: __ARM_FP 0x{{.*}}
// V8M_BASELINE-NOT: __GCC_HAVE_SYNC_COMPARE_AND_SWAP_1

// RUN: %clang -target armv8m.main-none-linux-gnu -x c -E -dM %s -o - | FileCheck --check-prefix=V8M_MAINLINE %s
// V8M_MAINLINE: __ARM_ARCH 8
// V8M_MAINLINE: __ARM_ARCH_8M_MAIN__ 1
// V8M_MAINLINE: __ARM_ARCH_EXT_IDIV__ 1
// V8M_MAINLINE-NOT: __ARM_ARCH_ISA_ARM
// V8M_MAINLINE: __ARM_ARCH_ISA_THUMB 2
// V8M_MAINLINE: __ARM_ARCH_PROFILE 'M'
// V8M_MAINLINE-NOT: __ARM_FEATURE_CRC32
// V8M_MAINLINE-NOT: __ARM_FEATURE_DSP
// V8M_MAINLINE: __ARM_FP 0xE
// V8M_MAINLINE: __GCC_HAVE_SYNC_COMPARE_AND_SWAP_1 1

// RUN: %clang -target arm-none-linux-gnu -march=armv8-m.main+dsp -x c -E -dM %s -o - | FileCheck --check-prefix=V8M_MAINLINE_DSP %s
// V8M_MAINLINE_DSP: __ARM_ARCH 8
// V8M_MAINLINE_DSP: __ARM_ARCH_8M_MAIN__ 1
// V8M_MAINLINE_DSP: __ARM_ARCH_EXT_IDIV__ 1
// V8M_MAINLINE_DSP-NOT: __ARM_ARCH_ISA_ARM
// V8M_MAINLINE_DSP: __ARM_ARCH_ISA_THUMB 2
// V8M_MAINLINE_DSP: __ARM_ARCH_PROFILE 'M'
// V8M_MAINLINE_DSP-NOT: __ARM_FEATURE_CRC32
// V8M_MAINLINE_DSP: __ARM_FEATURE_DSP 1
// V8M_MAINLINE_DSP: __ARM_FP 0xE
// V8M_MAINLINE_DSP: __GCC_HAVE_SYNC_COMPARE_AND_SWAP_1 1

// RUN: %clang -target arm-none-linux-gnu -x c -E -dM %s -o - | FileCheck --check-prefix=CHECK-DEFS %s
// CHECK-DEFS:#define __ARM_PCS 1
// CHECK-DEFS:#define __ARM_SIZEOF_MINIMAL_ENUM 4
// CHECK-DEFS:#define __ARM_SIZEOF_WCHAR_T 4

// RUN: %clang -target arm-none-linux-gnu -fno-math-errno -fno-signed-zeros\
// RUN:        -fno-trapping-math -fassociative-math -freciprocal-math\
// RUN:        -x c -E -dM %s -o - | FileCheck --check-prefix=CHECK-FASTMATH %s
// RUN: %clang -target arm-none-linux-gnu -ffast-math -x c -E -dM %s -o -\
// RUN:        | FileCheck --check-prefix=CHECK-FASTMATH %s
// CHECK-FASTMATH: __ARM_FP_FAST 1

// RUN: %clang -target arm-none-linux-gnu -fshort-wchar -x c -E -dM %s -o - | FileCheck --check-prefix=CHECK-SHORTWCHAR %s
// CHECK-SHORTWCHAR:#define __ARM_SIZEOF_WCHAR_T 2

// RUN: %clang -target arm-none-linux-gnu -fshort-enums -x c -E -dM %s -o - | FileCheck --check-prefix=CHECK-SHORTENUMS %s
// CHECK-SHORTENUMS:#define __ARM_SIZEOF_MINIMAL_ENUM 1

// Test that -mhwdiv has the right effect for a target CPU which has hwdiv enabled by default.
// RUN: %clang -target armv7 -mcpu=cortex-a15 -x c -E -dM %s -o - | FileCheck --check-prefix=HWDIV %s
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-a15 -x c -E -dM %s -o - | FileCheck --check-prefix=HWDIV %s
// RUN: %clang -target armv7 -mcpu=cortex-a15 -mhwdiv=arm -x c -E -dM %s -o - | FileCheck --check-prefix=HWDIV %s
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-a15 -mhwdiv=thumb -x c -E -dM %s -o - | FileCheck --check-prefix=HWDIV %s
// HWDIV:#define __ARM_ARCH_EXT_IDIV__ 1

// RUN: %clang -target arm -mcpu=cortex-a15 -mhwdiv=thumb -x c -E -dM %s -o - | FileCheck --check-prefix=NOHWDIV %s
// RUN: %clang -target arm -mthumb -mcpu=cortex-a15 -mhwdiv=arm -x c -E -dM %s -o - | FileCheck --check-prefix=NOHWDIV %s
// RUN: %clang -target arm -mcpu=cortex-a15 -mhwdiv=none -x c -E -dM %s -o - | FileCheck --check-prefix=NOHWDIV %s
// RUN: %clang -target arm -mthumb -mcpu=cortex-a15 -mhwdiv=none -x c -E -dM %s -o - | FileCheck --check-prefix=NOHWDIV %s
// NOHWDIV-NOT:#define __ARM_ARCH_EXT_IDIV__


// Check that -mfpu works properly for Cortex-A7 (enabled by default).
// RUN: %clang -target armv7-none-linux-gnueabi -mcpu=cortex-a7 -x c -E -dM %s -o - | FileCheck --check-prefix=DEFAULTFPU-A7 %s
// RUN: %clang -target armv7-none-linux-gnueabi -mthumb -mcpu=cortex-a7 -x c -E -dM %s -o - | FileCheck --check-prefix=DEFAULTFPU-A7 %s
// DEFAULTFPU-A7:#define __ARM_FP 0xE
// DEFAULTFPU-A7:#define __ARM_NEON__ 1
// DEFAULTFPU-A7:#define __ARM_VFPV4__ 1

// RUN: %clang -target armv7-none-linux-gnueabi -mcpu=cortex-a7 -mfpu=none -x c -E -dM %s -o - | FileCheck --check-prefix=FPUNONE-A7 %s
// RUN: %clang -target armv7-none-linux-gnueabi -mthumb -mcpu=cortex-a7 -mfpu=none -x c -E -dM %s -o - | FileCheck --check-prefix=FPUNONE-A7 %s
// FPUNONE-A7-NOT:#define __ARM_FP 0x{{.*}}
// FPUNONE-A7-NOT:#define __ARM_NEON__ 1
// FPUNONE-A7-NOT:#define __ARM_VFPV4__ 1

// RUN: %clang -target armv7-none-linux-gnueabi -mcpu=cortex-a7 -mfpu=vfp4 -x c -E -dM %s -o - | FileCheck --check-prefix=NONEON-A7 %s
// RUN: %clang -target armv7-none-linux-gnueabi -mthumb -mcpu=cortex-a7 -mfpu=vfp4 -x c -E -dM %s -o - | FileCheck --check-prefix=NONEON-A7 %s
// NONEON-A7:#define __ARM_FP 0xE
// NONEON-A7-NOT:#define __ARM_NEON__ 1
// NONEON-A7:#define __ARM_VFPV4__ 1

// Check that -mfpu works properly for Cortex-A5 (enabled by default).
// RUN: %clang -target armv7-none-linux-gnueabi -mcpu=cortex-a5 -x c -E -dM %s -o - | FileCheck --check-prefix=DEFAULTFPU-A5 %s
// RUN: %clang -target armv7-none-linux-gnueabi -mthumb -mcpu=cortex-a5 -x c -E -dM %s -o - | FileCheck --check-prefix=DEFAULTFPU-A5 %s
// DEFAULTFPU-A5:#define __ARM_FP 0xE
// DEFAULTFPU-A5:#define __ARM_NEON__ 1
// DEFAULTFPU-A5:#define __ARM_VFPV4__ 1

// RUN: %clang -target armv7-none-linux-gnueabi -mcpu=cortex-a5 -mfpu=none -x c -E -dM %s -o - | FileCheck --check-prefix=FPUNONE-A5 %s
// RUN: %clang -target armv7-none-linux-gnueabi -mthumb -mcpu=cortex-a5 -mfpu=none -x c -E -dM %s -o - | FileCheck --check-prefix=FPUNONE-A5 %s
// FPUNONE-A5-NOT:#define __ARM_FP 0x{{.*}}
// FPUNONE-A5-NOT:#define __ARM_NEON__ 1
// FPUNONE-A5-NOT:#define __ARM_VFPV4__ 1

// RUN: %clang -target armv7-none-linux-gnueabi -mcpu=cortex-a5 -mfpu=vfp4-d16 -x c -E -dM %s -o - | FileCheck --check-prefix=NONEON-A5 %s
// RUN: %clang -target armv7-none-linux-gnueabi -mthumb -mcpu=cortex-a5 -mfpu=vfp4-d16 -x c -E -dM %s -o - | FileCheck --check-prefix=NONEON-A5 %s
// NONEON-A5:#define __ARM_FP 0xE
// NONEON-A5-NOT:#define __ARM_NEON__ 1
// NONEON-A5:#define __ARM_VFPV4__ 1

// FIXME: add check for further predefines
// Test whether predefines are as expected when targeting ep9312.
// RUN: %clang -target armv4t -mcpu=ep9312 -x c -E -dM %s -o - | FileCheck --check-prefix=A4T %s
// A4T-NOT:#define __ARM_FEATURE_DSP
// A4T-NOT:#define __ARM_FP 0x{{.*}}

// Test whether predefines are as expected when targeting arm10tdmi.
// RUN: %clang -target armv5 -mcpu=arm10tdmi -x c -E -dM %s -o - | FileCheck --check-prefix=A5T %s
// A5T-NOT:#define __ARM_FEATURE_DSP
// A5T-NOT:#define __ARM_FP 0x{{.*}}

// Test whether predefines are as expected when targeting cortex-a5.
// RUN: %clang -target armv7 -mcpu=cortex-a5 -x c -E -dM %s -o - | FileCheck --check-prefix=A5 %s
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-a5 -x c -E -dM %s -o - | FileCheck --check-prefix=A5 %s
// A5:#define __ARM_ARCH 7
// A5:#define __ARM_ARCH_7A__ 1
// A5-NOT:#define __ARM_ARCH_EXT_IDIV__
// A5:#define __ARM_ARCH_PROFILE 'A'
// A5-NOT: #define __ARM_FEATURE_DIRECTED_ROUNDING
// A5:#define __ARM_FEATURE_DSP
// A5-NOT: #define __ARM_FEATURE_NUMERIC_MAXMIN
// A5:#define __ARM_FP 0xE

// Test whether predefines are as expected when targeting cortex-a7.
// RUN: %clang -target armv7k -mcpu=cortex-a7 -x c -E -dM %s -o - | FileCheck --check-prefix=A7 %s
// RUN: %clang -target armv7k -mthumb -mcpu=cortex-a7 -x c -E -dM %s -o - | FileCheck --check-prefix=A7 %s
// A7:#define __ARM_ARCH 7
// A7:#define __ARM_ARCH_EXT_IDIV__ 1
// A7:#define __ARM_ARCH_PROFILE 'A'
// A7:#define __ARM_FEATURE_DSP
// A7:#define __ARM_FP 0xE

// Test whether predefines are as expected when targeting cortex-a8.
// RUN: %clang -target armv7 -mcpu=cortex-a8 -x c -E -dM %s -o - | FileCheck --check-prefix=A8 %s
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-a8 -x c -E -dM %s -o - | FileCheck --check-prefix=A8 %s
// A8-NOT:#define __ARM_ARCH_EXT_IDIV__
// A8:#define __ARM_FEATURE_DSP
// A8:#define __ARM_FP 0xC

// Test whether predefines are as expected when targeting cortex-a9.
// RUN: %clang -target armv7 -mcpu=cortex-a9 -x c -E -dM %s -o - | FileCheck --check-prefix=A9 %s
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-a9 -x c -E -dM %s -o - | FileCheck --check-prefix=A9 %s
// A9-NOT:#define __ARM_ARCH_EXT_IDIV__
// A9:#define __ARM_FEATURE_DSP
// A9:#define __ARM_FP 0xE


// Check that -mfpu works properly for Cortex-A12 (enabled by default).
// RUN: %clang -target armv7-none-linux-gnueabi -mcpu=cortex-a12 -x c -E -dM %s -o - | FileCheck --check-prefix=DEFAULTFPU-A12 %s
// RUN: %clang -target armv7-none-linux-gnueabi -mthumb -mcpu=cortex-a12 -x c -E -dM %s -o - | FileCheck --check-prefix=DEFAULTFPU-A12 %s
// DEFAULTFPU-A12:#define __ARM_FP 0xE
// DEFAULTFPU-A12:#define __ARM_NEON__ 1
// DEFAULTFPU-A12:#define __ARM_VFPV4__ 1

// RUN: %clang -target armv7-none-linux-gnueabi -mcpu=cortex-a12 -mfpu=none -x c -E -dM %s -o - | FileCheck --check-prefix=FPUNONE-A12 %s
// RUN: %clang -target armv7-none-linux-gnueabi -mthumb -mcpu=cortex-a12 -mfpu=none -x c -E -dM %s -o - | FileCheck --check-prefix=FPUNONE-A12 %s
// FPUNONE-A12-NOT:#define __ARM_FP 0x{{.*}}
// FPUNONE-A12-NOT:#define __ARM_NEON__ 1
// FPUNONE-A12-NOT:#define __ARM_VFPV4__ 1

// Test whether predefines are as expected when targeting cortex-a12.
// RUN: %clang -target armv7 -mcpu=cortex-a12 -x c -E -dM %s -o - | FileCheck --check-prefix=A12 %s
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-a12 -x c -E -dM %s -o - | FileCheck --check-prefix=A12 %s
// A12:#define __ARM_ARCH 7
// A12:#define __ARM_ARCH_7A__ 1
// A12:#define __ARM_ARCH_EXT_IDIV__ 1
// A12:#define __ARM_ARCH_PROFILE 'A'
// A12:#define __ARM_FEATURE_DSP
// A12:#define __ARM_FP 0xE

// Test whether predefines are as expected when targeting cortex-a15.
// RUN: %clang -target armv7 -mcpu=cortex-a15 -x c -E -dM %s -o - | FileCheck --check-prefix=A15 %s
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-a15 -x c -E -dM %s -o - | FileCheck --check-prefix=A15 %s
// A15:#define __ARM_ARCH_EXT_IDIV__ 1
// A15:#define __ARM_FEATURE_DSP
// A15:#define __ARM_FP 0xE

// Check that -mfpu works properly for Cortex-A17 (enabled by default).
// RUN: %clang -target armv7-none-linux-gnueabi -mcpu=cortex-a17 -x c -E -dM %s -o - | FileCheck --check-prefix=DEFAULTFPU-A17 %s
// RUN: %clang -target armv7-none-linux-gnueabi -mthumb -mcpu=cortex-a17 -x c -E -dM %s -o - | FileCheck --check-prefix=DEFAULTFPU-A17 %s
// DEFAULTFPU-A17:#define __ARM_FP 0xE
// DEFAULTFPU-A17:#define __ARM_NEON__ 1
// DEFAULTFPU-A17:#define __ARM_VFPV4__ 1

// RUN: %clang -target armv7-none-linux-gnueabi -mcpu=cortex-a17 -mfpu=none -x c -E -dM %s -o - | FileCheck --check-prefix=FPUNONE-A17 %s
// RUN: %clang -target armv7-none-linux-gnueabi -mthumb -mcpu=cortex-a17 -mfpu=none -x c -E -dM %s -o - | FileCheck --check-prefix=FPUNONE-A17 %s
// FPUNONE-A17-NOT:#define __ARM_FP 0x{{.*}}
// FPUNONE-A17-NOT:#define __ARM_NEON__ 1
// FPUNONE-A17-NOT:#define __ARM_VFPV4__ 1

// Test whether predefines are as expected when targeting cortex-a17.
// RUN: %clang -target armv7 -mcpu=cortex-a17 -x c -E -dM %s -o - | FileCheck --check-prefix=A17 %s
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-a17 -x c -E -dM %s -o - | FileCheck --check-prefix=A17 %s
// A17:#define __ARM_ARCH 7
// A17:#define __ARM_ARCH_7A__ 1
// A17:#define __ARM_ARCH_EXT_IDIV__ 1
// A17:#define __ARM_ARCH_PROFILE 'A'
// A17:#define __ARM_FEATURE_DSP
// A17:#define __ARM_FP 0xE

// Test whether predefines are as expected when targeting swift.
// RUN: %clang -target armv7s -mcpu=swift -x c -E -dM %s -o - | FileCheck --check-prefix=SWIFT %s
// RUN: %clang -target armv7s -mthumb -mcpu=swift -x c -E -dM %s -o - | FileCheck --check-prefix=SWIFT %s
// SWIFT:#define __ARM_ARCH_EXT_IDIV__ 1
// SWIFT:#define __ARM_FEATURE_DSP
// SWIFT:#define __ARM_FP 0xE

// Test whether predefines are as expected when targeting ARMv8-A Cortex implementations
// RUN: %clang -target armv8 -mcpu=cortex-a32 -x c -E -dM %s -o - | FileCheck --check-prefix=ARMV8 %s
// RUN: %clang -target armv8 -mthumb -mcpu=cortex-a32 -x c -E -dM %s -o - | FileCheck --check-prefix=ARMV8 %s
// RUN: %clang -target armv8 -mcpu=cortex-a53 -x c -E -dM %s -o - | FileCheck --check-prefix=ARMV8 %s
// RUN: %clang -target armv8 -mthumb -mcpu=cortex-a53 -x c -E -dM %s -o - | FileCheck --check-prefix=ARMV8 %s
// ARMV8:#define __ARM_ARCH_EXT_IDIV__ 1
// ARMV8:#define __ARM_FEATURE_DSP
// ARMV8:#define __ARM_FP 0xE

// Test whether predefines are as expected when targeting cortex-r4.
// RUN: %clang -target armv7 -mcpu=cortex-r4 -x c -E -dM %s -o - | FileCheck --check-prefix=R4-ARM %s
// R4-ARM-NOT:#define __ARM_ARCH_EXT_IDIV__
// R4-ARM:#define __ARM_FEATURE_DSP
// R4-ARM-NOT:#define __ARM_FP 0x{{.*}}

// RUN: %clang -target armv7 -mthumb -mcpu=cortex-r4 -x c -E -dM %s -o - | FileCheck --check-prefix=R4-THUMB %s
// R4-THUMB:#define __ARM_ARCH_EXT_IDIV__ 1
// R4-THUMB:#define __ARM_FEATURE_DSP
// R4-THUMB-NOT:#define __ARM_FP 0x{{.*}}

// Test whether predefines are as expected when targeting cortex-r4f.
// RUN: %clang -target armv7 -mcpu=cortex-r4f -x c -E -dM %s -o - | FileCheck --check-prefix=R4F-ARM %s
// R4F-ARM-NOT:#define __ARM_ARCH_EXT_IDIV__
// R4F-ARM:#define __ARM_FEATURE_DSP
// R4F-ARM:#define __ARM_FP 0xC

// RUN: %clang -target armv7 -mthumb -mcpu=cortex-r4f -x c -E -dM %s -o - | FileCheck --check-prefix=R4F-THUMB %s
// R4F-THUMB:#define __ARM_ARCH_EXT_IDIV__ 1
// R4F-THUMB:#define __ARM_FEATURE_DSP
// R4F-THUMB:#define __ARM_FP 0xC

// Test whether predefines are as expected when targeting cortex-r5.
// RUN: %clang -target armv7 -mcpu=cortex-r5 -x c -E -dM %s -o - | FileCheck --check-prefix=R5 %s
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-r5 -x c -E -dM %s -o - | FileCheck --check-prefix=R5 %s
// R5:#define __ARM_ARCH_EXT_IDIV__ 1
// R5:#define __ARM_FEATURE_DSP
// R5:#define __ARM_FP 0xC

// Test whether predefines are as expected when targeting cortex-r7 and cortex-r8.
// RUN: %clang -target armv7 -mcpu=cortex-r7 -x c -E -dM %s -o - | FileCheck --check-prefix=R7-R8 %s
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-r7 -x c -E -dM %s -o - | FileCheck --check-prefix=R7-R8 %s
// RUN: %clang -target armv7 -mcpu=cortex-r8 -x c -E -dM %s -o - | FileCheck --check-prefix=R7-R8 %s
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-r8 -x c -E -dM %s -o - | FileCheck --check-prefix=R7-R8 %s
// R7-R8:#define __ARM_ARCH_EXT_IDIV__ 1
// R7-R8:#define __ARM_FEATURE_DSP
// R7-R8:#define __ARM_FP 0xE

// Test whether predefines are as expected when targeting cortex-m0.
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-m0 -x c -E -dM %s -o - | FileCheck --check-prefix=M0-THUMB %s
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-m0plus -x c -E -dM %s -o - | FileCheck --check-prefix=M0-THUMB %s
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-m1 -x c -E -dM %s -o - | FileCheck --check-prefix=M0-THUMB %s
// RUN: %clang -target armv7 -mthumb -mcpu=sc000 -x c -E -dM %s -o - | FileCheck --check-prefix=M0-THUMB %s
// M0-THUMB-NOT:#define __ARM_ARCH_EXT_IDIV__
// M0-THUMB-NOT:#define __ARM_FEATURE_DSP
// M0-THUMB-NOT:#define __ARM_FP 0x{{.*}}

// Test whether predefines are as expected when targeting cortex-m3.
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-m3 -x c -E -dM %s -o - | FileCheck --check-prefix=M3-THUMB %s
// RUN: %clang -target armv7 -mthumb -mcpu=sc300 -x c -E -dM %s -o - | FileCheck --check-prefix=M3-THUMB %s
// M3-THUMB:#define __ARM_ARCH_EXT_IDIV__ 1
// M3-THUMB-NOT:#define __ARM_FEATURE_DSP
// M3-THUMB-NOT:#define __ARM_FP 0x{{.*}}

// Test whether predefines are as expected when targeting cortex-m4.
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-m4 -x c -E -dM %s -o - | FileCheck --check-prefix=M4-THUMB %s
// M4-THUMB:#define __ARM_ARCH_EXT_IDIV__ 1
// M4-THUMB:#define __ARM_FEATURE_DSP
// M4-THUMB:#define __ARM_FP 0x6

// Test whether predefines are as expected when targeting cortex-m7.
// RUN: %clang -target armv7 -mthumb -mcpu=cortex-m7 -x c -E -dM %s -o - | FileCheck --check-prefix=M7-THUMB %s
// M7-THUMB:#define __ARM_ARCH_EXT_IDIV__ 1
// M7-THUMB:#define __ARM_FEATURE_DSP
// M7-THUMB:#define __ARM_FP 0xE

// Test whether predefines are as expected when targeting krait.
// RUN: %clang -target armv7 -mcpu=krait -x c -E -dM %s -o - | FileCheck --check-prefix=KRAIT %s
// RUN: %clang -target armv7 -mthumb -mcpu=krait -x c -E -dM %s -o - | FileCheck --check-prefix=KRAIT %s
// KRAIT:#define __ARM_ARCH_EXT_IDIV__ 1
// KRAIT:#define __ARM_FEATURE_DSP
// KRAIT:#define  __ARM_VFPV4__ 1

// RUN: %clang -target armv8.1a-none-none-eabi -x c -E -dM %s -o - | FileCheck --check-prefix=CHECK-V81A %s
// CHECK-V81A: __ARM_ARCH 8
// CHECK-V81A: __ARM_ARCH_8_1A__ 1
// CHECK-V81A: #define __ARM_ARCH_PROFILE 'A'
// CHECK-V81A: __ARM_FEATURE_QRDMX 1
// CHECK-V81A: #define __ARM_FP 0xE

// RUN: %clang -target armv8.2a-none-none-eabi -x c -E -dM %s -o - | FileCheck --check-prefix=CHECK-V82A %s
// CHECK-V82A: __ARM_ARCH 8
// CHECK-V82A: __ARM_ARCH_8_2A__ 1
// CHECK-V82A: #define __ARM_ARCH_PROFILE 'A'
// CHECK-V82A: #define __ARM_FP 0xE
