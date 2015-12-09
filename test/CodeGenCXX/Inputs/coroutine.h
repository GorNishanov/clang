// coroutine support header

namespace std {
template <typename R, typename...> struct coroutine_traits {
  using promise_type = typename R::promise_type;
};

template <typename Promise = void> struct coroutine_handle;

template <> struct coroutine_handle<void> {
  static coroutine_handle from_address(void *addr) {
    coroutine_handle me;
    me.ptr = addr;
    return me;
  }
  void *address() const { return ptr; }
  void resume() const { __builtin_coro_resume(ptr); }
  void destroy() const { __builtin_coro_destroy(ptr); }
  bool done() const { return __builtin_coro_done(ptr); }
  coroutine_handle &operator=(decltype(nullptr)) {
    ptr = nullptr;
    return *this;
  }
  coroutine_handle() : ptr(nullptr) {}
  explicit operator bool() const { return ptr; }

protected:
  void *ptr;
};

template <typename Promise> struct coroutine_handle : coroutine_handle<> {
  static constexpr auto _Offset() {
    // FIXME: add llvm.coro.offset intrinsic
    struct Fake {
      void *a;
      void *b;
      int c;
      Promise p;
    };
    Fake x;
    return reinterpret_cast<char *>(&x.p) - reinterpret_cast<char *>(&x);
  }
  using coroutine_handle<>::operator=;
  Promise &promise() const {
    return *reinterpret_cast<Promise *>(static_cast<char *>(ptr) + _Offset());
  }
  static coroutine_handle from_address(void *addr) {
    coroutine_handle me;
    me.ptr = addr;
    return me;
  }
  static coroutine_handle from_promise(Promise &promise) {
    coroutine_handle p;
    p.ptr = reinterpret_cast<char *>(&promise) - _Offset();
    return p;
  }
};

struct suspend_always {
  bool await_ready() { return false; }
  void await_suspend(coroutine_handle<>) {}
  void await_resume() {}
};
struct suspend_never {
  bool await_ready() { return true; }
  void await_suspend(coroutine_handle<>) {}
  void await_resume() {}
};
}

// FIXME: generate in the FE?
template <typename Awaitable, typename Promise> void _Ramp(void *a, void *b) {
  static_cast<Awaitable *>(a)->await_suspend(std::coroutine_handle<Promise>::from_address(b)); 
}
