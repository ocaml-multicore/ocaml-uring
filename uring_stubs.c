/*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
 * Copyright (C) 2020-2021 Sadiq Jaffer
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <liburing.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <string.h>

#undef URING_DEBUG
#ifdef URING_DEBUG
#define dprintf(fmt, ...) fprintf(stderr, fmt, ##__VA_ARGS__)
#else
#define dprintf(fmt, ...) ((void)0)
#endif

#define Ring_val(v) *((struct io_uring**)Data_custom_val(v))

static struct custom_operations ring_ops = {
  "uring.ring",
  custom_finalize_default, /* TODO: Finalize should check we've taken down the ring and if not, take it down */
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value ocaml_uring_setup(value entries) {
  CAMLparam1(entries);
  CAMLlocal1(v_uring);

  struct io_uring* ring = (struct io_uring*)caml_stat_alloc(sizeof(struct io_uring));

  int status = io_uring_queue_init(Long_val(entries), ring, 0);

  if (status == 0) {
      v_uring = caml_alloc_custom_mem(&ring_ops, sizeof(struct io_uring*), sizeof(struct io_uring));
      Ring_val(v_uring) = ring;
      CAMLreturn(v_uring);
  } else
     unix_error(-status, "io_uring_queue_init", Nothing);
}

value ocaml_uring_register_ba(value v_uring, value v_ba) {
  CAMLparam2(v_uring, v_ba);
  struct io_uring *ring = Ring_val(v_uring);
  struct iovec iov[1];
  iov[0].iov_base = Caml_ba_data_val(v_ba);
  iov[0].iov_len = Caml_ba_array_val(v_ba)->dim[0];
  dprintf("uring %p: registering iobuf base %p len %lu\n", ring, iov[0].iov_base, iov[0].iov_len);
  int ret = io_uring_register_buffers(ring, iov, 1);
  if (ret)
    unix_error(-ret, "io_uring_register_buffers", Nothing);
  CAMLreturn(Val_unit);
}

value ocaml_uring_unregister_ba(value v_uring, value v_ba) {
  CAMLparam2(v_uring, v_ba);
  struct io_uring *ring = Ring_val(v_uring);
  dprintf("uring %p: unregistering buffer");
  int ret = io_uring_unregister_buffers(ring);
  if (ret)
    unix_error(-ret, "io_uring_register_buffers", Nothing);
  CAMLreturn(Val_unit);
}

value ocaml_uring_exit(value v_uring) {
  CAMLparam1(v_uring);
  struct io_uring *ring = Ring_val(v_uring);
  dprintf("uring %p: exit\n", ring);
  if (ring) {
    io_uring_queue_exit(ring);
    caml_stat_free(ring);
    Ring_val(v_uring) = NULL;
  }
  CAMLreturn(Val_unit);
}

value
ocaml_uring_alloc_iovecs(value v_ba_arr) {
  CAMLparam1(v_ba_arr);
  size_t len = Wosize_val(v_ba_arr);
  struct iovec *iovs = caml_stat_alloc(len * sizeof(struct iovec));
  for (int i=0; i<len; i++) {
    value v_ba = Field(v_ba_arr,i);
    iovs[i].iov_base = Caml_ba_data_val(v_ba);
    iovs[i].iov_len = Caml_ba_array_val(v_ba)->dim[0];
    dprintf( "iov %d: %p %lu\n", i, iovs[i].iov_base, iovs[i].iov_len);
  }
  if (((uintptr_t) iovs & 1) == 1) caml_failwith("unaligned alloc??"); 
  CAMLreturn ((value) iovs | 1);
}

value
ocaml_iovec_advance_offset(value v_iovecs, value v_idx, value v_adj)
{
  struct iovec *iovs = (struct iovec *) (v_iovecs & ~1);
  int idx = Int_val(v_idx);
  int adj = Int_val(v_adj);
  iovs[idx].iov_base += adj;
  iovs[idx].iov_len -= adj;
  return(Val_unit);
}

value
ocaml_uring_free_iovecs(value iovecs)
{
   struct iovec *iovs = (struct iovec *) (iovecs & ~1);
   free (iovs);
   iovs = NULL;
   return(Val_unit);
}

value
ocaml_uring_submit_readv(value v_uring, value v_fd, value v_id, value v_iov, value v_off) {
  CAMLparam1(v_uring);
  struct io_uring *ring = Ring_val(v_uring);
  struct iovec *iovs = (struct iovec *) (Field(v_iov, 0)  & ~1);
  int len = Wosize_val(Field(v_iov, 1));
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  if (!sqe) CAMLreturn(Val_false);
  dprintf("submit_readv: %d ents len[0] %lu off %d\n", len, iovs[0].iov_len, Int_val(v_off));
  io_uring_prep_readv(sqe, Int_val(v_fd), iovs, len, Int_val(v_off));
  io_uring_sqe_set_data(sqe, (void *)(uintptr_t)Int_val(v_id)); /* TODO sort out cast */
  CAMLreturn(Val_true);
}

value
ocaml_uring_submit_writev(value v_uring, value v_fd, value v_id, value v_iov, value v_off) {
  CAMLparam1(v_uring);
  struct io_uring *ring = Ring_val(v_uring);
  struct iovec *iovs = (struct iovec *) (Field(v_iov, 0) & ~1);
  int len = Wosize_val(Field(v_iov, 1));
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  if (!sqe) CAMLreturn(Val_false);
  dprintf("submit_writev: %d ents len[0] %lu off %d\n", len, iovs[0].iov_len, Int_val(v_off));
  io_uring_prep_writev(sqe, Int_val(v_fd), iovs, len, Int_val(v_off));
  io_uring_sqe_set_data(sqe, (void *)(uintptr_t)Int_val(v_id)); /* TODO sort out cast */
  CAMLreturn(Val_true);
}

value
ocaml_uring_submit_readv_fixed_native(value v_uring, value v_fd, value v_id, value v_ba, value v_off, value v_len, value v_fileoff) {
  CAMLparam2(v_uring, v_ba);
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  void *buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
  if (!sqe) CAMLreturn(Val_false);
  dprintf("submit_readv_fixed: buf %p off %d len %d fileoff %d", buf, Int_val(v_off), Int_val(v_len), Int_val(v_fileoff));
  io_uring_prep_read_fixed(sqe, Int_val(v_fd), buf, Int_val(v_len), Int_val(v_fileoff), 0);
  io_uring_sqe_set_data(sqe, (void *)(uintptr_t)Int_val(v_id)); /* TODO sort out cast */
  CAMLreturn(Val_true);
}

value
ocaml_uring_submit_writev_fixed_native(value v_uring, value v_fd, value v_id, value v_ba, value v_off, value v_len, value v_fileoff) {
  CAMLparam2(v_uring, v_ba);
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  void *buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
  if (!sqe)
    CAMLreturn(Val_false);
  dprintf("submit_writev_fixed: buf %p off %d len %d fileoff %d", buf, Int_val(v_off), Int_val(v_len), Int_val(v_fileoff));
  io_uring_prep_write_fixed(sqe, Int_val(v_fd), buf, Int_val(v_len), Int_val(v_fileoff), 0);
  io_uring_sqe_set_data(sqe, (void *)(uintptr_t)Int_val(v_id)); /* TODO sort out cast */
  CAMLreturn(Val_true);
}

value ocaml_uring_submit(value v_uring)
{
  CAMLparam1(v_uring);
  struct io_uring *ring = Ring_val(v_uring);
  int num = io_uring_submit(ring);
  CAMLreturn(Val_int(num));
}

value ocaml_uring_wait_cqe_timeout(value v_timeout, value v_uring)
{
  CAMLparam2(v_uring, v_timeout);
  CAMLlocal1(v_ret);
  double timeout = Double_val(v_timeout);
  struct __kernel_timespec t;
  t.tv_sec = (time_t) timeout;
  t.tv_nsec = (timeout - t.tv_sec) * 1e9;
  long id;
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_cqe *cqe;
  int res;
  dprintf("cqe: waiting, timeout %fs\n", timeout);
  res = io_uring_wait_cqe_timeout(ring, &cqe, &t);
  if (res < 0) {
    v_ret = caml_alloc(2, 0);
    Store_field(v_ret, 0, Val_int(-1));
    Store_field(v_ret, 1, Val_int(res));
  } else {
    id = (long)io_uring_cqe_get_data(cqe);
    io_uring_cqe_seen(ring, cqe);
    v_ret = caml_alloc(2, 0);
    Store_field(v_ret, 0, Val_int(id));
    Store_field(v_ret, 1, Val_int(cqe->res));
  }
  CAMLreturn(v_ret);
}

value ocaml_uring_wait_cqe(value v_uring)
{
  CAMLparam1(v_uring);
  CAMLlocal1(v_ret);
  long id;
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_cqe *cqe;
  int res;
  dprintf("cqe: waiting\n");
  res = io_uring_wait_cqe(ring, &cqe);
  if (res < 0) {
    v_ret = caml_alloc(2, 0);
    Store_field(v_ret, 0, Val_int(-1));
    Store_field(v_ret, 1, Val_int(res));
  } else {
    id = (long)io_uring_cqe_get_data(cqe);
    io_uring_cqe_seen(ring, cqe);
    v_ret = caml_alloc(2, 0);
    Store_field(v_ret, 0, Val_int(id));
    Store_field(v_ret, 1, Val_int(cqe->res));
  }
  CAMLreturn(v_ret);
}

value ocaml_uring_peek_cqe(value v_uring)
{
  CAMLparam1(v_uring);
  CAMLlocal1(v_ret);
  long id;
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_cqe *cqe;
  int res;
  dprintf("cqe: peeking\n");
  res = io_uring_peek_cqe(ring, &cqe);
  if (res < 0) {
    v_ret = caml_alloc(2, 0);
    Store_field(v_ret, 0, Val_int(-1));
    Store_field(v_ret, 1, Val_int(res));
  } else {
    id = (long)io_uring_cqe_get_data(cqe);
    io_uring_cqe_seen(ring, cqe);
    v_ret = caml_alloc(2, 0);
    Store_field(v_ret, 0, Val_int(id));
    Store_field(v_ret, 1, Val_int(cqe->res));
  }
  CAMLreturn(v_ret);
}
