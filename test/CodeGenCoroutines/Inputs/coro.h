void free(void *ptr);
void *malloc(unsigned int);

#define CORO_SUSPEND_IMPL(IsFinal)                                             \
  switch (__builtin_coro_suspend(__builtin_coro_save(0), IsFinal)) {           \
  case 0:                                                                      \
    if (IsFinal)                                                               \
      __builtin_trap();                                                        \
    break;                                                                     \
  case 1:                                                                      \
    goto coro_Cleanup;                                                         \
  default:                                                                     \
    goto coro_Suspend;                                                         \
  }

#define CORO_SUSPEND() CORO_SUSPEND_IMPL(0)
#define CORO_FINAL_SUSPEND() CORO_SUSPEND_IMPL(1)

#define CORO_BEGIN(AllocFunc)                                                  \
  void *coro_hdl = __builtin_coro_begin(__builtin_coro_id(0, 0, 0, 0),         \
                                        AllocFunc(__builtin_coro_size()));

#define CORO_END(FreeFunc)                                                     \
  coro_Cleanup : {                                                             \
    void *coro_mem = __builtin_coro_free(coro_hdl);                            \
    if (coro_mem)                                                              \
      FreeFunc(coro_mem);                                                      \
  }                                                                            \
  coro_Suspend:                                                                \
  __builtin_coro_end(0, 0);                                                    \
  return coro_hdl;

#define CORO_RESUME(hdl) __builtin_coro_resume(hdl)
#define CORO_DESTROY(hdl) __builtin_coro_destroy(hdl)
