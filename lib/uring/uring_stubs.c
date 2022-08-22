/*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
 * Copyright (C) 2020-2021 Sadiq Jaffer
 * Copyright (C) 2022 Christiano Haesbaert
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

#include <endian.h>	/* liburing.h needs this for __BYTE_ORDER */
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
#include <caml/socketaddr.h>
#include <sys/socket.h>
#include <errno.h>
#include <string.h>
#include <poll.h>
#include <sys/uio.h>
#include <fcntl.h>
#include <unistd.h>

#undef URING_DEBUG
#ifdef URING_DEBUG
#define dprintf(fmt, ...) fprintf(stderr, fmt, ##__VA_ARGS__)
#else
#define dprintf(fmt, ...) ((void)0)
#endif

// TODO: this belongs in Optint
#ifdef ARCH_SIXTYFOUR
#define Int63_val(v) Long_val(v)
#else
#define Int63_val(v) (Int64_val(v)) >> 1
#endif

#define Ring_val(v) *((struct io_uring**)Data_custom_val(v))
#define Sketch_ptr_val(vsp) (Caml_ba_data_val(Field(vsp, 0)) + Long_val(Field(vsp, 1)))
#define Sketch_ptr_len_val(vsp) Long_val(Field(vsp, 2))

// Note that this does not free the ring data. You must not allow this to be
// GC'd until the ring has been released by calling ocaml_uring_exit.
static struct custom_operations ring_ops = {
  "uring.ring",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value ocaml_uring_setup(value entries, value polling_timeout) {
  CAMLparam1(entries);
  CAMLlocal1(v_uring);
  struct io_uring_params params;

  v_uring = caml_alloc_custom_mem(&ring_ops, sizeof(struct io_uring*), sizeof(struct io_uring));
  Ring_val(v_uring) = NULL;

  // On OOM, this raises. [v_uring] will be freed by the GC.
  struct io_uring* ring = (struct io_uring*)caml_stat_alloc(sizeof(struct io_uring));
  Ring_val(v_uring) = ring;

  memset(&params, 0, sizeof(params));
  if (Is_some(polling_timeout)) {
    params.flags |= IORING_SETUP_SQPOLL;
    params.sq_thread_idle = Int_val(Some_val(polling_timeout));
  }
  int status = io_uring_queue_init_params(Long_val(entries), ring, &params);

  if (status == 0) {
    CAMLreturn(v_uring);
  } else {
    caml_stat_free(ring);
    // The GC will free [v_uring].
    unix_error(-status, "io_uring_queue_init", Nothing);
  }
}

// Note that the ring must be idle when calling this.
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

// Note that the ring must be idle when calling this.
value ocaml_uring_unregister_buffers(value v_uring) {
  CAMLparam1(v_uring);
  struct io_uring *ring = Ring_val(v_uring);
  dprintf("uring %p: unregistering buffers");
  int ret = io_uring_unregister_buffers(ring);
  if (ret)
    unix_error(-ret, "io_uring_register_buffers", Nothing);
  CAMLreturn(Val_unit);
}

// Note that the ring must be idle when calling this.
value ocaml_uring_exit(value v_uring) {
  CAMLparam1(v_uring);
  struct io_uring *ring = Ring_val(v_uring);
  dprintf("uring %p: exit\n", ring);
  if (ring) {
    io_uring_queue_exit(ring);
    caml_stat_free(ring);
    Ring_val(v_uring) = NULL;
    // Can now allow the GC to release v_uring
  }
  CAMLreturn(Val_unit);
}

value /* noalloc */
ocaml_uring_submit_nop(value v_uring, value v_id) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  if (!sqe) return (Val_false);
  io_uring_prep_nop(sqe);
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

value /* noalloc */
ocaml_uring_sq_ready(value v_uring) {
  struct io_uring *ring = Ring_val(v_uring);
  return (Val_int(io_uring_sq_ready(ring)));
}

void /* noalloc */
ocaml_uring_set_timespec(value v_sketch_ptr, value v_timeout)
{
  struct __kernel_timespec *t = Sketch_ptr_val(v_sketch_ptr);
  t->tv_sec = 0;
  t->tv_nsec = Int64_val(v_timeout);
}

#define Val_boottime Val_int(0)

value /* noalloc */
ocaml_uring_submit_timeout(value v_uring, value v_id, value v_sketch_ptr, value v_clock, value v_absolute)
{
  struct __kernel_timespec *t = Sketch_ptr_val(v_sketch_ptr);
  struct io_uring* ring = Ring_val(v_uring);
  struct io_uring_sqe* sqe;
  int flags;

  if (v_clock == Val_boottime)
    flags = IORING_TIMEOUT_BOOTTIME;
  else
    flags = IORING_TIMEOUT_REALTIME;

  if(Bool_val(v_absolute))
    flags |= IORING_TIMEOUT_ABS;

  sqe = io_uring_get_sqe(ring);
  if (!sqe) return Val_false;
  io_uring_prep_timeout(sqe, t, 0, flags);
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return Val_true;
}

struct open_how_data {
  struct open_how how;
  char path[];
};

#define Open_how_val(v) (*((struct open_how_data **) Data_custom_val(v)))

static void finalize_open_how(value v) {
  caml_stat_free(Open_how_val(v));
  Open_how_val(v) = NULL;
}

static struct custom_operations open_how_ops = {
  "uring.open_how",
  finalize_open_how,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

// v_path is copied into the returned block.
value
ocaml_uring_make_open_how(value v_flags, value v_mode, value v_resolve, value v_path) {
  CAMLparam1(v_path);
  CAMLlocal1(v);
  if (!caml_string_is_c_safe(v_path))
    caml_invalid_argument("ocaml_uring_make_open_how: path is not C-safe");
  int path_len = caml_string_length(v_path) + 1;
  struct open_how_data *data;
  v = caml_alloc_custom_mem(&open_how_ops, sizeof(struct open_how_data *), sizeof(struct open_how_data) + path_len);
  Open_how_val(v) = NULL;
  data = (struct open_how_data *) caml_stat_alloc(sizeof(struct open_how_data) + path_len);
  data->how.flags = Long_val(v_flags);
  data->how.mode = Long_val(v_mode);
  data->how.resolve = Long_val(v_resolve);
  memcpy(data->path, String_val(v_path), path_len);
  Open_how_val(v) = data;
  CAMLreturn(v);
}

// Caller must ensure v_open_how is not GC'd until the job is finished.
value /* noalloc */
ocaml_uring_submit_openat2(value v_uring, value v_id, value v_fd, value v_open_how) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  if (!sqe) return (Val_false);
  struct open_how_data *data = Open_how_val(v_open_how);
  io_uring_prep_openat2(sqe, Int_val(v_fd), data->path, &data->how);
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

value /* noalloc */
ocaml_uring_submit_close(value v_uring, value v_fd, value v_id) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  if (!sqe) return (Val_false);
  dprintf("submit_close: fd:%d\n", Int_val(v_fd));
  io_uring_prep_close(sqe, Int_val(v_fd));
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

value /* noalloc */
ocaml_uring_submit_poll_add(value v_uring, value v_fd, value v_id, value v_poll_mask) {
  int poll_mask = Int_val(v_poll_mask);
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  if (!sqe) return (Val_false);
  dprintf("submit_poll_add: fd:%d mask:%x\n", Int_val(v_fd), poll_mask);
  io_uring_prep_poll_add(sqe, Int_val(v_fd), poll_mask);
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

void /* noalloc */
ocaml_uring_set_iovec(value v_sketch_ptr, value v_csl)
{
  int i;
  value v_aux;
  struct iovec *iovs = Sketch_ptr_val(v_sketch_ptr);

  for (i = 0, v_aux = v_csl;
       v_aux != Val_emptylist;
       v_aux = Field(v_aux, 1), i++) {
    value v_cs = Field(v_aux, 0);
    value v_ba = Field(v_cs, 0);
    value v_off = Field(v_cs, 1);
    value v_len = Field(v_cs, 2);
    iovs[i].iov_base = Caml_ba_data_val(v_ba) + Long_val(v_off);
    iovs[i].iov_len = Long_val(v_len);
  }
}

// Caller must ensure the buffers pointed to by v_sketch_ptr are not GC'd until the job is finished.
value /* noalloc */
ocaml_uring_submit_readv(value v_uring, value v_fd, value v_id, value v_sketch_ptr, value v_fileoff) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  struct iovec *iovs = Sketch_ptr_val(v_sketch_ptr);
  size_t len = Sketch_ptr_len_val(v_sketch_ptr) / sizeof(*iovs);

  if (sqe == NULL)
    return (Val_false);
  dprintf("submit_readv: %d ents len[0] %lu off %d\n", len, iovs[0].iov_len, Int63_val(v_fileoff));
  io_uring_prep_readv(sqe, Int_val(v_fd), iovs, len, Int63_val(v_fileoff));
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

// Caller must ensure the buffers pointed to by v_sketch_ptr are not GC'd until the job is finished.
value /* noalloc */
ocaml_uring_submit_writev(value v_uring, value v_fd, value v_id, value v_sketch_ptr, value v_fileoff) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  struct iovec *iovs = Sketch_ptr_val(v_sketch_ptr);
  size_t len = Sketch_ptr_len_val(v_sketch_ptr) / sizeof(*iovs);

  if (sqe == NULL)
    return (Val_false);
  dprintf("submit_writev: %d ents len[0] %lu off %d\n", len, iovs[0].iov_len, Int63_val(v_fileoff));
  io_uring_prep_writev(sqe, Int_val(v_fd), iovs, len, Int63_val(v_fileoff));
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

// Caller must ensure the buffers are not released until this job completes.
value /* noalloc */
ocaml_uring_submit_readv_fixed_native(value v_uring, value v_fd, value v_id, value v_ba, value v_off, value v_len, value v_fileoff) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  void *buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
  if (!sqe) return (Val_false);
  dprintf("submit_readv_fixed: buf %p off %d len %d fileoff %d", buf, Int_val(v_off), Int_val(v_len), Int63_val(v_fileoff));
  io_uring_prep_read_fixed(sqe, Int_val(v_fd), buf, Int_val(v_len), Int63_val(v_fileoff), 0);
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

value
ocaml_uring_submit_readv_fixed_byte(value* values, int argc) {
  return ocaml_uring_submit_readv_fixed_native(
			  values[0],
			  values[1],
			  values[2],
			  values[3],
			  values[4],
			  values[5],
			  values[6]);
}

// Caller must ensure the buffers are not released until this job completes.
value /* noalloc */
ocaml_uring_submit_writev_fixed_native(value v_uring, value v_fd, value v_id, value v_ba, value v_off, value v_len, value v_fileoff) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  void *buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
  if (!sqe)
    return (Val_false);
  dprintf("submit_writev_fixed: buf %p off %d len %d fileoff %d", buf, Int_val(v_off), Int_val(v_len), Int63_val(v_fileoff));
  io_uring_prep_write_fixed(sqe, Int_val(v_fd), buf, Int_val(v_len), Int63_val(v_fileoff), 0);
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

value
ocaml_uring_submit_writev_fixed_byte(value* values, int argc) {
  return ocaml_uring_submit_writev_fixed_native(
			  values[0],
			  values[1],
			  values[2],
			  values[3],
			  values[4],
			  values[5],
			  values[6]);
}

value /* noalloc */
ocaml_uring_submit_read(value v_uring, value v_fd, value v_id, value v_cstruct, value v_fileoff) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  value v_ba = Field(v_cstruct, 0);
  value v_off = Field(v_cstruct, 1);
  value v_len = Field(v_cstruct, 2);
  void *buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
  if (!sqe) return (Val_false);
  dprintf("submit_read: fd %d buff %p len %zd fileoff %d\n",
	  Int_val(v_fd), buf, Long_val(v_len), Int63_val(v_fileoff));
  io_uring_prep_read(sqe, Int_val(v_fd), buf, Long_val(v_len), Int63_val(v_fileoff));
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

value /* noalloc */
ocaml_uring_submit_write(value v_uring, value v_fd, value v_id, value v_cstruct, value v_fileoff) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  value v_ba = Field(v_cstruct, 0);
  value v_off = Field(v_cstruct, 1);
  value v_len = Field(v_cstruct, 2);
  void *buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
  if (!sqe) return (Val_false);
  dprintf("submit_write: fd %d buff %p len %zd fileoff %d\n",
	  Int_val(v_fd), buf, Long_val(v_len), Int63_val(v_fileoff));
  io_uring_prep_write(sqe, Int_val(v_fd), buf, Long_val(v_len), Int63_val(v_fileoff));
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

value /* noalloc */
ocaml_uring_submit_splice(value v_uring, value v_id, value v_fd_in, value v_fd_out, value v_nbytes) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  if (!sqe) return (Val_false);
  io_uring_prep_splice(sqe,
		       Int_val(v_fd_in), (int64_t) -1,
		       Int_val(v_fd_out), (int64_t) -1,
		       Int_val(v_nbytes), 0);
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

struct sock_addr_data {
  union sock_addr_union sock_addr_addr;
  socklen_param_type sock_addr_len;
};

#define Sock_addr_val(v) (*((struct sock_addr_data **) Data_custom_val(v)))

static void finalize_sock_addr(value v) {
  caml_stat_free(Sock_addr_val(v));
  Sock_addr_val(v) = NULL;
}

static struct custom_operations sockaddr_ops = {
  "uring.sockaddr",
  finalize_sock_addr,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value
ocaml_uring_make_sockaddr(value v_sockaddr) {
  CAMLparam1(v_sockaddr);
  CAMLlocal1(v);
  struct sock_addr_data *data;
  v = caml_alloc_custom_mem(&sockaddr_ops, sizeof(struct sock_addr_data *), sizeof(struct sock_addr_data));
  Sock_addr_val(v) = NULL;
  data = (struct sock_addr_data *) caml_stat_alloc(sizeof(struct sock_addr_data));
  Sock_addr_val(v) = data;
  // If this raises, the GC will free [v], which will free [data]:
  get_sockaddr(v_sockaddr, &data->sock_addr_addr, &data->sock_addr_len);
  CAMLreturn(v);
}

value
ocaml_uring_extract_sockaddr(value v) {
  CAMLparam1(v);
  CAMLlocal1(v_sockaddr);
  struct sock_addr_data *data = Sock_addr_val(v);
  v_sockaddr = alloc_sockaddr(&data->sock_addr_addr, data->sock_addr_len, -1);
  CAMLreturn(v_sockaddr);
}

#define Msghdr_val(v) (*((struct msghdr **) Data_custom_val(v)))

static void finalize_msghdr(value v) {
  caml_stat_free(Msghdr_val(v));
  Msghdr_val(v) = NULL;
}

static struct custom_operations msghdr_ops = {
  "uring.msghdr_ops",
  finalize_msghdr,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

struct msghdr_with_cmsg {
  struct msghdr msg;
  struct cmsghdr cmsg;
};

// v_sockaddr must not be freed before the msghdr as it contains pointers to them
value /* v_n_fds can go away, count ourselves */
ocaml_uring_make_msghdr(value v_n_fds, value v_fds, value v_sockaddr_opt) {
  CAMLparam2(v_fds, v_sockaddr_opt);
  CAMLlocal1(v);
  struct msghdr *msg;
  int n_fds = Int_val(v_n_fds);
  int cmsg_offset, controllen, total_size;
  cmsg_offset = sizeof(struct msghdr_with_cmsg) - sizeof(struct cmsghdr);
  controllen = n_fds > 0 ? CMSG_SPACE(sizeof(int) * n_fds) : 0;
  total_size = cmsg_offset + controllen;
  //dprintf("using %d bytes to hold %d FDs\n", total_size, n_fds);
  // Allocate a pointer on the OCaml heap for the msghdr
  v = caml_alloc_custom_mem(&msghdr_ops, sizeof(struct msghdr *), total_size);
  Msghdr_val(v) = NULL;
  msg = (struct msghdr *) caml_stat_alloc(total_size);
  // The msghdr and cmsghdr must zero-ed to avoid unwanted errors
  memset(msg, 0, total_size);
  Msghdr_val(v) = msg;
  if (Is_some(v_sockaddr_opt)) {
    struct sock_addr_data *addr = Sock_addr_val(Some_val(v_sockaddr_opt));
    // Store the address data in the message
    msg->msg_name = &(addr->sock_addr_addr);
    msg->msg_namelen = sizeof(addr->sock_addr_addr);
  } else {
    msg->msg_name = NULL;
    msg->msg_namelen = 0;
  }
  msg->msg_iov = NULL;          /* will be filled in sendmsg or recvmsg */
  msg->msg_iovlen = 0;          /* will be filled in sendmsg or recvmsg */
  // Add the FDs to the message
  if (n_fds > 0) {
    int i;
    struct cmsghdr *cm;
    msg->msg_control = &(((struct msghdr_with_cmsg *) msg)->cmsg);
    msg->msg_controllen = controllen;
    if (Is_block(v_fds)) {
      cm = CMSG_FIRSTHDR(msg);
      cm->cmsg_level = SOL_SOCKET;
      cm->cmsg_type = SCM_RIGHTS;
      cm->cmsg_len = CMSG_LEN(n_fds * sizeof(int));
      for (i = 0; i < n_fds; i++) {
	int fd = -1;
	if (Is_block(v_fds)) {
	  fd = Int_val(Field(v_fds, 0));
	  v_fds = Field(v_fds, 1);
	}
	((int *)CMSG_DATA(cm))[i] = fd;
      }
    }
  }
  CAMLreturn(v);
}

value
ocaml_uring_get_msghdr_fds(value v_msghdr) {
  CAMLparam1(v_msghdr);
  CAMLlocal2(v_list, v_cons);
  struct msghdr *msg = Msghdr_val(v_msghdr);
  struct cmsghdr *cm;
  v_list = Val_int(0);
  for (cm = CMSG_FIRSTHDR(msg); cm; cm = CMSG_NXTHDR(msg, cm)) {
    if (cm->cmsg_level == SOL_SOCKET && cm->cmsg_type == SCM_RIGHTS) {
      int *fds = (int *) CMSG_DATA(cm);
      int n_fds = (cm->cmsg_len - CMSG_LEN(0)) / sizeof(int);
      int i;
      for (i = n_fds - 1; i >= 0; i--) {
	int fd = Val_int(fds[i]);
	value v_cons = caml_alloc_tuple(2);
	Store_field(v_cons, 0, fd);
	Store_field(v_cons, 1, v_list);
	v_list = v_cons;
      }
    }
  }
  CAMLreturn(v_list);
}

// v_sockaddr must not be GC'd while the call is in progress
value /* noalloc */
ocaml_uring_submit_connect(value v_uring, value v_id, value v_fd, value v_sockaddr) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe;
  struct sock_addr_data *addr = Sock_addr_val(v_sockaddr);
  sqe = io_uring_get_sqe(ring);
  if (!sqe) return (Val_false);
  io_uring_prep_connect(sqe, Int_val(v_fd), &(addr->sock_addr_addr.s_gen), addr->sock_addr_len);
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

// v_msghdr must not be GC'd while the call is in progress
value /* noalloc */
ocaml_uring_submit_send_msg(value v_uring, value v_id, value v_fd, value v_msghdr, value v_sketch_ptr) {
  struct io_uring *ring = Ring_val(v_uring);
  struct msghdr *msg = Msghdr_val(Field(v_msghdr, 0));
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  if (sqe == NULL) return (Val_false);
  msg->msg_iov = Sketch_ptr_val(v_sketch_ptr);
  msg->msg_iovlen = Sketch_ptr_len_val(v_sketch_ptr) / sizeof(struct iovec);
  dprintf("submit_sendmsg\n");
  io_uring_prep_sendmsg(sqe, Int_val(v_fd), msg, 0);
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

// v_msghdr must not be GC'd while the call is in progress
value /* noalloc */
ocaml_uring_submit_recv_msg(value v_uring, value v_id, value v_fd, value v_msghdr, value v_sketch_ptr) {
  struct io_uring *ring = Ring_val(v_uring);
  struct msghdr *msg = Msghdr_val(Field(v_msghdr, 0));
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  if (sqe == NULL) return (Val_false);
  msg->msg_iov = Sketch_ptr_val(v_sketch_ptr);
  msg->msg_iovlen = Sketch_ptr_len_val(v_sketch_ptr) / sizeof(struct iovec);
  dprintf("submit_recvmsg:msghdr %p: registering iobuf base %p len %lu\n", msg, msg->msg_iov[0].iov_base, msg->msg_iov[0].iov_len);
  io_uring_prep_recvmsg(sqe, Int_val(v_fd), msg, 0);
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

// v_sockaddr must not be GC'd while the call is in progress
value /* noalloc */
ocaml_uring_submit_accept(value v_uring, value v_id, value v_fd, value v_sockaddr) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe;
  struct sock_addr_data *addr = Sock_addr_val(v_sockaddr);
  addr->sock_addr_len = sizeof(union sock_addr_union);
  sqe = io_uring_get_sqe(ring);
  if (!sqe) return (Val_false);
  io_uring_prep_accept(sqe, Int_val(v_fd), &(addr->sock_addr_addr.s_gen), &addr->sock_addr_len, SOCK_CLOEXEC);
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

value /* noalloc */
ocaml_uring_set_string(value v_sketch_ptr, value v_string)
{
  char *dst = Sketch_ptr_val(v_sketch_ptr);
  strcpy(dst, String_val(v_string));
  return (Val_unit);
}

// Caller must ensure the path pointed to by v_sketch_ptr is not GC'd until the job is finished.
value /* noalloc */
ocaml_uring_submit_unlinkat(value v_uring, value v_id, value v_fd, value v_sketch_ptr, value v_rmdir) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
  int flags = Bool_val(v_rmdir) ? AT_REMOVEDIR : 0;
  char *path = Sketch_ptr_val(v_sketch_ptr);
  if (!sqe) return (Val_false);
  io_uring_prep_unlinkat(sqe, Int_val(v_fd), path, flags);
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

value /* noalloc */
ocaml_uring_submit_cancel(value v_uring, value v_id, value v_target) {
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_sqe *sqe;
  sqe = io_uring_get_sqe(ring);
  if (!sqe) return (Val_false);
  io_uring_prep_cancel(sqe, (void *)Long_val(v_target), 0);
  io_uring_sqe_set_data(sqe, (void *)Long_val(v_id));
  return (Val_true);
}

value /* noalloc */
ocaml_uring_submit(value v_uring)
{
  struct io_uring *ring = Ring_val(v_uring);
  int num = io_uring_submit(ring);
  return (Val_int(num));
}

#define Val_cqe_none Val_int(0)

static value Val_cqe_some(value id, value res) {
  CAMLparam2(id, res);
  CAMLlocal1(some);
  some = caml_alloc(2, 0);
  Store_field(some, 0, id);
  Store_field(some, 1, res);
  CAMLreturn(some);
}

value ocaml_uring_wait_cqe_timeout(value v_timeout, value v_uring)
{
  CAMLparam2(v_uring, v_timeout);
  double timeout = Double_val(v_timeout);
  struct __kernel_timespec t;
  t.tv_sec = (time_t) timeout;
  t.tv_nsec = (timeout - t.tv_sec) * 1e9;
  long id;
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_cqe *cqe;
  int res;
  dprintf("cqe: waiting, timeout %fs\n", timeout);
  caml_enter_blocking_section();
  io_uring_submit(ring);
  res = io_uring_wait_cqe_timeout(ring, &cqe, &t);
  caml_leave_blocking_section();
  if (res < 0) {
    if (res == -EAGAIN || res == -EINTR || res == -ETIME) {
      CAMLreturn(Val_cqe_none);
    } else {
      unix_error(-res, "io_uring_wait_cqe_timeout", Nothing);
    }
  } else {
    id = (long)io_uring_cqe_get_data(cqe);
    io_uring_cqe_seen(ring, cqe);
    CAMLreturn(Val_cqe_some(Val_int(id), Val_int(cqe->res)));
  }
}

value ocaml_uring_wait_cqe(value v_uring)
{
  CAMLparam1(v_uring);
  long id;
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_cqe *cqe;
  int res;
  dprintf("cqe: waiting\n");
  caml_enter_blocking_section();
  io_uring_submit(ring);
  res = io_uring_wait_cqe(ring, &cqe);
  caml_leave_blocking_section();
  if (res < 0) {
    if (res == -EAGAIN || res == -EINTR) {
      CAMLreturn(Val_cqe_none);
    } else {
      unix_error(-res, "io_uring_wait_cqe", Nothing);
    }
  } else {
    id = (long)io_uring_cqe_get_data(cqe);
    io_uring_cqe_seen(ring, cqe);
    CAMLreturn(Val_cqe_some(Val_int(id), Val_int(cqe->res)));
  }
}

value ocaml_uring_peek_cqe(value v_uring)
{
  CAMLparam1(v_uring);
  long id;
  struct io_uring *ring = Ring_val(v_uring);
  struct io_uring_cqe *cqe;
  int res;
  dprintf("cqe: peeking\n");
  res = io_uring_peek_cqe(ring, &cqe);
  if (res < 0) {
    if (res == -EAGAIN || res == -EINTR) {
      CAMLreturn(Val_cqe_none);
    } else {
      unix_error(-res, "io_uring_peek_cqe", Nothing);
    }
  } else {
    id = (long)io_uring_cqe_get_data(cqe);
    io_uring_cqe_seen(ring, cqe);
    CAMLreturn(Val_cqe_some(Val_int(id), Val_int(cqe->res)));
  }
}

// Allocates
value ocaml_uring_error_of_errno(value v_errno) {
  return unix_error_of_code(Int_val(v_errno));
}
