(*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
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
 *)

module Private = struct
  module Heap = Heap
end

module Region = Region
module Int63 = Optint.Int63

module type FLAGS = sig
  type t = private int
  val of_int : int -> t
  val ( + ) : t -> t -> t
  val mem : t -> t -> bool
end

module Flags = struct
  type t = int

  let empty = 0

  let of_int x = x

  let ( + ) = ( lor )

  let mem a b =
    (a land b) = a
end

module Open_flags = struct
  include Flags

  let rdonly    = Config.o_rdonly
  let wronly    = Config.o_wronly
  let rdwr      = Config.o_rdwr

  let creat     = Config.o_creat
  let excl      = Config.o_excl
  let noctty    = Config.o_noctty
  let trunc     = Config.o_trunc
  let append    = Config.o_append
  let nonblock  = Config.o_nonblock
  let dsync     = Config.o_dsync
  let direct    = Config.o_direct
  let largefile = Config.o_largefile
  let directory = Config.o_directory
  let nofollow  = Config.o_nofollow
  let noatime   = Config.o_noatime
  let cloexec   = Config.o_cloexec
  let sync      = Config.o_sync
  let path      = Config.o_path
  let tmpfile   = Config.o_tmpfile
end

module Resolve = struct
  include Flags

  let no_xdev       = 0x01
  let no_magiclinks = 0x02
  let no_symlinks   = 0x04
  let beneath       = 0x08
  let in_root       = 0x10
  let cached        = 0x20
end

module Poll_mask = struct
  include Flags

  let pollin  = Config.pollin
  let pollout = Config.pollout
  let pollerr = Config.pollerr
  let pollhup = Config.pollhup
end

module Sockaddr = struct
  type t

  external of_unix : Unix.sockaddr -> t = "ocaml_uring_make_sockaddr"
  external get : t -> Unix.sockaddr = "ocaml_uring_extract_sockaddr"

  let dummy_addr = Unix.ADDR_UNIX "-"

  let create () = of_unix dummy_addr
end

module Open_how = struct
  type t

  external make : int -> Unix.file_perm -> int -> string -> t = "ocaml_uring_make_open_how"

  let v ~open_flags ~perm ~resolve path = make open_flags perm resolve path
end

(* The C stubs rely on the layout of Cstruct.t, so we just check here that it hasn't changed. *)
module Check_cstruct : sig
  [@@@warning "-34"]
  type t = private {
    buffer: (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
    off   : int;
    len   : int;
  }
end = Cstruct

(*
 * A Sketch buffer is an area used to hold objects that remain alive
 * until the next `Uring.submit`.
 * For example an `iovec` must be passed to io_uring in `readv` and
 * `writev`, once we call `Uring.submit` the `iovec` structures are
 * copied by the kernel and we can release them, which we do.
 *)
module Sketch = struct
  type t = {
    mutable buffer : Cstruct.buffer;
    mutable off : int;
    mutable old_buffers : Cstruct.buffer list;
  }

  type ptr = Cstruct.buffer * int * int

  let create_buffer len = Bigarray.(Array1.create char c_layout len)

  let create len =
    { buffer = create_buffer len; off = 0; old_buffers = [] }

  let length t = Bigarray.Array1.size_in_bytes t.buffer

  let round a x = (x + (a - 1)) land (lnot (a - 1))
  let round = round (Sys.word_size / 8)

  let avail t = (length t) - t.off

  let alloc t alloc_len =
    let alloc_len = round alloc_len in
    if alloc_len > avail t then begin
      let new_buffer = create_buffer ((length t) + alloc_len) in
      t.old_buffers <- t.buffer :: t.old_buffers;
      t.off <- 0;
      t.buffer <- new_buffer;
    end;
    let off = t.off in
    t.off <- t.off + alloc_len;
    (t.buffer, off, alloc_len)

  let _cstruct_of_ptr ((buf, off, len) : ptr) =
    Cstruct.of_bigarray buf ~off ~len

  let release t =
    t.off <- 0;
    t.old_buffers <- []

  module Iovec = struct
    external set : ptr -> Cstruct.t list -> unit = "ocaml_uring_set_iovec" [@@noalloc]

    let sizeof = Config.sizeof_iovec

    let alloc t csl =
      let ptr = alloc t (List.length csl * sizeof) in
      set ptr csl;
      ptr
  end
end

(* Used for the sendmsg/recvmsg calls. Liburing doesn't support sendto/recvfrom at the time of writing. *)
module Msghdr = struct
  type msghdr
  type t = msghdr * Sockaddr.t option * Cstruct.t list (* `Cstruct.t list` is here only for preventing it being GCed *)
  external make_msghdr : int -> Unix.file_descr list -> Sockaddr.t option -> msghdr = "ocaml_uring_make_msghdr"
  external get_msghdr_fds : msghdr -> Unix.file_descr list = "ocaml_uring_get_msghdr_fds"

  let get_fds (hdr, _, _) = get_msghdr_fds hdr

  (* Create a value with space for [n_fds] file descriptors.
     When sending, [fds] is used to fill those slots. When receiving, they can be left blank. *)
  let create_with_addr ~n_fds ~fds ?addr buffs =
    make_msghdr n_fds fds addr, addr, buffs

  let create ?(n_fds=0) ?addr buffs =
    create_with_addr ~n_fds ~fds:[] ?addr buffs
end

type 'a job = 'a Heap.entry

module Uring = struct
  type t

  external create : int -> int option -> t = "ocaml_uring_setup"
  external exit : t -> unit = "ocaml_uring_exit"

  external unregister_buffers : t -> unit = "ocaml_uring_unregister_buffers"
  external register_bigarray : t ->  Cstruct.buffer -> unit = "ocaml_uring_register_ba"
  external submit : t -> int = "ocaml_uring_submit" [@@noalloc]
  external sq_ready : t -> int = "ocaml_uring_sq_ready" [@@noalloc]

  type id = Heap.ptr

  type offset = Optint.Int63.t
  external submit_nop : t -> id -> bool = "ocaml_uring_submit_nop" [@@noalloc]
  external submit_poll_add : t -> Unix.file_descr -> id -> Poll_mask.t -> bool = "ocaml_uring_submit_poll_add" [@@noalloc]
  external submit_read : t -> Unix.file_descr -> id -> Cstruct.t -> offset -> bool = "ocaml_uring_submit_read" [@@noalloc]
  external submit_write : t -> Unix.file_descr -> id -> Cstruct.t -> offset -> bool = "ocaml_uring_submit_write" [@@noalloc]
  external submit_readv : t -> Unix.file_descr -> id -> Sketch.ptr -> offset -> bool = "ocaml_uring_submit_readv" [@@noalloc]
  external submit_writev : t -> Unix.file_descr -> id -> Sketch.ptr -> offset -> bool = "ocaml_uring_submit_writev" [@@noalloc]
  external submit_readv_fixed : t -> Unix.file_descr -> id -> Cstruct.buffer -> int -> int -> offset -> bool = "ocaml_uring_submit_readv_fixed_byte" "ocaml_uring_submit_readv_fixed_native" [@@noalloc]
  external submit_writev_fixed : t -> Unix.file_descr -> id -> Cstruct.buffer -> int -> int -> offset -> bool = "ocaml_uring_submit_writev_fixed_byte" "ocaml_uring_submit_writev_fixed_native" [@@noalloc]
  external submit_close : t -> Unix.file_descr -> id -> bool = "ocaml_uring_submit_close" [@@noalloc]
  external submit_splice : t -> id -> Unix.file_descr -> Unix.file_descr -> int -> bool = "ocaml_uring_submit_splice" [@@noalloc]
  external submit_connect : t -> id -> Unix.file_descr -> Sockaddr.t -> bool = "ocaml_uring_submit_connect" [@@noalloc]
  external submit_accept : t -> id -> Unix.file_descr -> Sockaddr.t -> bool = "ocaml_uring_submit_accept" [@@noalloc]
  external submit_cancel : t -> id -> id -> bool = "ocaml_uring_submit_cancel" [@@noalloc]
  external submit_openat2 : t -> id -> Unix.file_descr -> Open_how.t -> bool = "ocaml_uring_submit_openat2" [@@noalloc]
  external submit_send_msg : t -> id -> Unix.file_descr -> Msghdr.t -> Sketch.ptr -> bool = "ocaml_uring_submit_send_msg" [@@noalloc]
  external submit_recv_msg : t -> id -> Unix.file_descr -> Msghdr.t -> Sketch.ptr -> bool = "ocaml_uring_submit_recv_msg" [@@noalloc]

  type cqe_option = private
    | Cqe_none
    | Cqe_some of { user_data_id : id; res: int }
  [@@ocaml.warning "-37" (* Avoids "Unused constructor" warning on OCaml <= 4.09. *)]

  external wait_cqe : t -> cqe_option = "ocaml_uring_wait_cqe"
  external wait_cqe_timeout : float -> t -> cqe_option = "ocaml_uring_wait_cqe_timeout"
  external peek_cqe : t -> cqe_option = "ocaml_uring_peek_cqe"

  external error_of_errno : int -> Unix.error = "ocaml_uring_error_of_errno"
end

type 'a t = {
  id : < >;
  uring: Uring.t;
  mutable fixed_iobuf: Cstruct.buffer;
  data : 'a Heap.t;
  sketch : Sketch.t;
  queue_depth: int;
  mutable dirty: bool; (* has outstanding requests that need to be submitted *)
}

module Generic_ring = struct
  type ring = T : 'a t -> ring
  type t = ring
  let compare (T a) (T b) = compare a.id b.id
end

module Ring_set = Set.Make(Generic_ring)

(* Garbage collection and buffers shared with the Linux kernel.

   Many uring operations involve passing Linux the address of a buffer to which it
   should write the results. This means that both Linux and OCaml have pointers to the
   buffer, and it must not be freed until both have finished with it, but the OCaml
   garbage collector doesn't know this. To avoid OCaml's GC freeing the buffer while
   Linux is still using it:

   - We attach all such buffers to their [t.data] entry, so they don't get freed until
     the job is complete, even if the caller loses interest in the buffer.

   - We add the ring itself to the global [gc_roots] set, so that [t.data] can't be freed
     unless [exit] is called, which checks that there are no operations in progress. *)
let gc_roots = Atomic.make Ring_set.empty

let rec update_gc_roots fn =
  let old_set = Atomic.get gc_roots in
  let new_set = fn old_set in
  if not (Atomic.compare_and_set gc_roots old_set new_set) then
    update_gc_roots fn

let register_gc_root t =
  update_gc_roots (Ring_set.add (Generic_ring.T t))

let unregister_gc_root t =
  update_gc_roots (Ring_set.remove (Generic_ring.T t))

let create ?polling_timeout ~queue_depth () =
  if queue_depth < 1 then Fmt.invalid_arg "Non-positive queue depth: %d" queue_depth;
  let uring = Uring.create queue_depth polling_timeout in
  let data = Heap.create queue_depth in
  let id = object end in
  let fixed_iobuf = Cstruct.empty.buffer in
  let sketch = Sketch.create 0 in
  let t = { id; uring; sketch; fixed_iobuf; data; dirty=false; queue_depth } in
  register_gc_root t;
  t

let ensure_idle t op =
  match Heap.in_use t.data with
  | 0 -> ()
  | n -> Fmt.invalid_arg "%s: %d request(s) still active!" op n

let set_fixed_buffer t iobuf =
  ensure_idle t "set_fixed_buffer";
  if Bigarray.Array1.dim t.fixed_iobuf > 0 then
    Uring.unregister_buffers t.uring;
  t.fixed_iobuf <- iobuf;
  if Bigarray.Array1.dim iobuf > 0 then (
    match Uring.register_bigarray t.uring iobuf with
    | () -> Ok ()
    | exception Unix.Unix_error(Unix.ENOMEM, "io_uring_register_buffers", "") -> Error `ENOMEM
  ) else Ok ()

let exit t =
  ensure_idle t "exit";
  Uring.exit t.uring;
  unregister_gc_root t

let with_id_full : type a. a t -> (Heap.ptr -> bool) -> a -> extra_data:'b -> a job option =
 fun t fn datum ~extra_data ->
  match Heap.alloc t.data datum ~extra_data with
  | exception Heap.No_space -> None
  | entry ->
    let ptr = Heap.ptr entry in
    let has_space = fn ptr in
    if has_space then (
      t.dirty <- true;
      Some entry
    ) else (
      ignore (Heap.free t.data ptr : a);
      None
    )

let with_id t fn a = with_id_full t fn a ~extra_data:()

let noop t user_data =
  with_id t (fun id -> Uring.submit_nop t.uring id) user_data

let at_fdcwd : Unix.file_descr = Obj.magic Config.at_fdcwd

let openat2 t ~access ~flags ~perm ~resolve ?(fd=at_fdcwd) path user_data =
  let open_flags = flags lor match access with
    | `R  -> Open_flags.rdonly
    | `W  -> Open_flags.wronly
    | `RW -> Open_flags.rdwr
  in
  let open_how = Open_how.v ~open_flags ~perm ~resolve path in
  with_id_full t (fun id -> Uring.submit_openat2 t.uring id fd open_how) user_data ~extra_data:open_how

let read t ~file_offset fd (buf : Cstruct.t) user_data =
  with_id_full t (fun id -> Uring.submit_read t.uring fd id buf file_offset) user_data ~extra_data:buf

let write t ~file_offset fd (buf : Cstruct.t) user_data =
  with_id_full t (fun id -> Uring.submit_write t.uring fd id buf file_offset) user_data ~extra_data:buf

let readv t ~file_offset fd buffers user_data =
  with_id_full t (fun id ->
      let iovec = Sketch.Iovec.alloc t.sketch buffers in
      Uring.submit_readv t.uring fd id iovec file_offset) user_data ~extra_data:buffers

let read_fixed t ~file_offset fd ~off ~len user_data =
  with_id t (fun id -> Uring.submit_readv_fixed t.uring fd id t.fixed_iobuf off len file_offset) user_data

let read_chunk ?len t ~file_offset fd chunk user_data =
  let { Cstruct.buffer; off; len } = Region.to_cstruct ?len chunk in
  if buffer != t.fixed_iobuf then invalid_arg "Chunk does not belong to ring!";
  with_id t (fun id -> Uring.submit_readv_fixed t.uring fd id t.fixed_iobuf off len file_offset) user_data

let write_fixed t ~file_offset fd ~off ~len user_data =
  with_id t (fun id -> Uring.submit_writev_fixed t.uring fd id t.fixed_iobuf off len file_offset) user_data

let write_chunk ?len t ~file_offset fd chunk user_data =
  let { Cstruct.buffer; off; len } = Region.to_cstruct ?len chunk in
  if buffer != t.fixed_iobuf then invalid_arg "Chunk does not belong to ring!";
  with_id t (fun id -> Uring.submit_writev_fixed t.uring fd id t.fixed_iobuf off len file_offset) user_data

let writev t ~file_offset fd buffers user_data =
  with_id_full t (fun id ->
      let iovec = Sketch.Iovec.alloc t.sketch buffers in
      Uring.submit_writev t.uring fd id iovec file_offset) user_data ~extra_data:buffers

let poll_add t fd poll_mask user_data =
  with_id t (fun id -> Uring.submit_poll_add t.uring fd id poll_mask) user_data

let close t fd user_data =
  with_id t (fun id -> Uring.submit_close t.uring fd id) user_data

let splice t ~src ~dst ~len user_data =
  with_id t (fun id -> Uring.submit_splice t.uring id src dst len) user_data

let connect t fd addr user_data =
  let addr = Sockaddr.of_unix addr in
  with_id_full t (fun id -> Uring.submit_connect t.uring id fd addr) user_data ~extra_data:addr

let accept t fd addr user_data =
  with_id_full t (fun id -> Uring.submit_accept t.uring id fd addr) user_data ~extra_data:addr

let send_msg ?(fds=[]) ?dst t fd buffers user_data =
  let addr = Option.map Sockaddr.of_unix dst in
  let n_fds = List.length fds in
  let msghdr = Msghdr.create_with_addr ~n_fds ~fds ?addr buffers in
  (* NOTE: `msghdr` references `buffers`, so it's enough for `extra_data` *)
  with_id_full t (fun id ->
      let iovec = Sketch.Iovec.alloc t.sketch buffers in
      Uring.submit_send_msg t.uring id fd msghdr iovec) user_data ~extra_data:msghdr

let recv_msg t fd msghdr user_data =
  let _, _, buffers = msghdr in
  (* NOTE: `msghdr` references `buffers`, so it's enough for `extra_data` *)
  with_id_full t (fun id ->
      let iovec = Sketch.Iovec.alloc t.sketch buffers in
      Uring.submit_recv_msg t.uring id fd msghdr iovec) user_data ~extra_data:msghdr

let cancel t job user_data =
  ignore (Heap.ptr job : Uring.id);  (* Check it's still valid *)
  with_id t (fun id -> Uring.submit_cancel t.uring id (Heap.ptr job)) user_data

let submit t =
  let v =
    if t.dirty then begin
      t.dirty <- false;
      Uring.submit t.uring
    end else
      0
  in
  if Uring.sq_ready t.uring = 0 then
    Sketch.release t.sketch;
  v

type 'a completion_option =
  | None
  | Some of { result: int; data: 'a }

let fn_on_ring fn t =
  match fn t.uring with
  | Uring.Cqe_none -> None
  | Uring.Cqe_some { user_data_id; res } ->
    let data = Heap.free t.data user_data_id in
    Some { result = res; data }

let peek t = fn_on_ring Uring.peek_cqe t

let wait ?timeout t =
  match timeout with
  | None -> fn_on_ring Uring.wait_cqe t
  | Some timeout -> fn_on_ring (Uring.wait_cqe_timeout timeout) t

let queue_depth {queue_depth;_} = queue_depth
let buf {fixed_iobuf;_} = fixed_iobuf

let error_of_errno e =
  Uring.error_of_errno (abs e)
