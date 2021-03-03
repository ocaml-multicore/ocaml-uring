#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <sys/uio.h>

#undef IOVEC_DEBUG
#ifdef IOVEC_DEBUG
#define dprintf(fmt, ...) fprintf(stderr, fmt, ##__VA_ARGS__)
#else
#define dprintf(fmt, ...) ((void)0)
#endif

value ocaml_iovec_alloc(value v_ba_arr)
{
  CAMLparam1(v_ba_arr);
  size_t len = Wosize_val(v_ba_arr);
  struct iovec *iovs = caml_stat_alloc(len * sizeof(struct iovec));
  for (int i = 0; i < len; i++)
  {
    value v_ba = Field(v_ba_arr, i);
    iovs[i].iov_base = Caml_ba_data_val(v_ba);
    iovs[i].iov_len = Caml_ba_array_val(v_ba)->dim[0];
    dprintf( "iov %d: %p %lu\n", i, iovs[i].iov_base, iovs[i].iov_len);
  }
  if (((uintptr_t)iovs & 1) == 1)
    caml_failwith("unaligned alloc??");
  CAMLreturn((value)iovs | 1);
}

value ocaml_iovec_advance(value v_iovecs, value v_idx, value v_adj)
{
  struct iovec *iovs = (struct iovec *)(v_iovecs & ~1);
  int idx = Int_val(v_idx);
  int adj = Int_val(v_adj);
  iovs[idx].iov_base += adj;
  iovs[idx].iov_len -= adj;
  return (Val_unit);
}

value ocaml_iovec_free(value iovecs)
{
  struct iovec *iovs = (struct iovec *)(iovecs & ~1);
  free(iovs);
  iovs = NULL;
  return (Val_unit);
}
