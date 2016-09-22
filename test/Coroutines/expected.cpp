// RUN: %clang_cc1 -triple x86_64-unknown-linux-gnu -fcoroutines -emit-llvm %s -o - -std=c++14 -O3 | FileCheck %s
#include "Inputs/coroutine.h"

namespace std {
template <class _Ty> struct remove_reference { typedef _Ty type; };

template <class _Ty> struct remove_reference<_Ty &> { typedef _Ty type; };

template <class _Ty> struct remove_reference<_Ty &&> { typedef _Ty type; };

template <class _Ty>
inline constexpr typename remove_reference<_Ty>::type &&
move(_Ty &&_Arg) noexcept {
  return (static_cast<typename remove_reference<_Ty>::type &&>(_Arg));
}
}

struct error {};

template <typename T, typename Error = int>
struct expected {

  struct Data {
    T val;
    Error error;
  };
  Data data;

  struct DataPtr {
    Data *p;
  };

  expected() {}
  expected(T val) : data{std::move(val),{}} {}
  expected(struct error, Error error) : data{{}, std::move(error)} {}
  expected(DataPtr p) : data{std::move(p.p->val), std::move(p.p->error)} {}

  struct promise_type {
    Data data;
    DataPtr get_return_object() { return {&data}; }
    std::suspend_never initial_suspend() { return {}; }
    std::suspend_never final_suspend() { return {}; }
    void return_value(T v) { data.val = std::move(v); data.error = {};}
  };

  bool await_ready() { return !data.error; }
  T await_resume() { return std::move(data.val); }
  void await_suspend(std::coroutine_handle<promise_type> h) {
    h.promise().data.error =std::move(data.error);
    h.destroy();
  }

  T const& value() { return data.val; }
  Error const& error() { return data.error; }
};

extern "C" expected<int> g() { return {0}; }
extern "C" expected<int> h() { return {error{}, 42}; }

extern "C" void print(int);

void WORKAROUND_FOR_SEMACOROUTINE_BUG() {
  std::coroutine_handle<expected<int>::promise_type>::from_address(0);
}

extern "C" expected<int> f1() {
  print(11);
  co_await g();
  print(12);
  co_return 100;
}

extern "C" expected<int> f2() {
  print(21);
  co_await h();
  print(22);
  co_return 200;
}

// CHECK-LABEL: @main(
int main() {
// CHECK: call void @print(i32 11)
// CHECK: call void @print(i32 12)
// CHECK: call void @print(i32 100)
// CHECK: call void @print(i32 0)
 
  auto c1 = f1();
  print(c1.value());
  print(c1.error());

// CHECK: call void @print(i32 21)
// CHECK: call void @print(i32
// CHECK: call void @print(i32 42)
  auto c2 = f2();
  print(c2.value());
  print(c2.error());
}
