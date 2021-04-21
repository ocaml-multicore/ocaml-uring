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

module Poll_mask = struct
  type t = int

  let pollin  = Config.pollin
  let pollout = Config.pollout
  let pollerr = Config.pollerr
  let pollhup = Config.pollhup

  let of_int x = x

  let ( + ) = ( lor )

  let mem a b =
    (a land b) = a
end

module Uring = struct
  type t
  external create : int -> t = "ocaml_uring_setup"
  external exit : t -> unit = "ocaml_uring_exit"

  external unregister_bigarray : t -> unit = "ocaml_uring_unregister_ba"
  external register_bigarray : t ->  Iovec.Buffer.t -> unit = "ocaml_uring_register_ba"
  external submit : t -> int = "ocaml_uring_submit"

  type id = Heap.ptr
  type offset = Optint.Int63.t
  external submit_nop : t -> id -> bool = "ocaml_uring_submit_nop" [@@noalloc]
  external submit_poll_add : t -> Unix.file_descr -> id -> Poll_mask.t -> bool = "ocaml_uring_submit_poll_add" [@@noalloc]
  external submit_readv : t -> Unix.file_descr -> id -> Iovec.t -> offset -> bool = "ocaml_uring_submit_readv" [@@noalloc]
  external submit_writev : t -> Unix.file_descr -> id -> Iovec.t -> offset -> bool = "ocaml_uring_submit_writev" [@@noalloc]
  external submit_readv_fixed : t -> Unix.file_descr -> id -> Iovec.Buffer.t -> int -> int -> offset -> bool = "ocaml_uring_submit_readv_fixed_byte" "ocaml_uring_submit_readv_fixed_native" [@@noalloc]
  external submit_writev_fixed : t -> Unix.file_descr -> id -> Iovec.Buffer.t -> int -> int -> offset -> bool = "ocaml_uring_submit_writev_fixed_byte" "ocaml_uring_submit_writev_fixed_native" [@@noalloc]
  external submit_close : t -> Unix.file_descr -> id -> bool = "ocaml_uring_submit_close" [@@noalloc]

  external wait_cqe : t -> id * int = "ocaml_uring_wait_cqe"
  external wait_cqe_timeout : float -> t -> id * int = "ocaml_uring_wait_cqe_timeout"
  external peek_cqe : t -> id * int = "ocaml_uring_peek_cqe"

  external error_of_errno : int -> Unix.error = "ocaml_uring_error_of_errno"
end

type 'a t = {
  uring: Uring.t;
  mutable fixed_iobuf: Iovec.Buffer.t;
  user_data : 'a Heap.t;
  queue_depth: int;
  mutable dirty: bool; (* has outstanding requests that need to be submitted *)
}

let default_iobuf_len = 1024 * 1024 (* 1MB *)

let create ?(fixed_buf_len=default_iobuf_len) ~queue_depth () =
  if queue_depth < 1 then Fmt.invalid_arg "Non-positive queue depth: %d" queue_depth;
  let uring = Uring.create queue_depth in
  (* TODO posix memalign this to page *)
  let fixed_iobuf = Iovec.Buffer.create fixed_buf_len in
  Uring.register_bigarray uring fixed_iobuf;
  Gc.finalise Uring.exit uring;
  let user_data = Heap.create queue_depth in
  { uring; fixed_iobuf; user_data; dirty=false; queue_depth }

let realloc t iobuf =
  Uring.unregister_bigarray t.uring;
  t.fixed_iobuf <- iobuf;
  Uring.register_bigarray t.uring iobuf

let exit {uring;_} = Uring.exit uring

let with_id : type a. a t -> (Heap.ptr -> bool) -> a -> bool =
 fun t fn datum ->
  match Heap.alloc t.user_data datum with
  | exception Heap.No_space -> false
  | ptr ->
     let has_space = fn ptr in
     if has_space then t.dirty <- true else ignore (Heap.free t.user_data ptr : a);
     has_space

let noop t user_data =
  with_id t (fun id -> Uring.submit_nop t.uring id) user_data

let readv t ?(offset=Int63.zero) fd iovec user_data =
  with_id t (fun id -> Uring.submit_readv t.uring fd id iovec offset) user_data

let read t ?(file_offset=Int63.zero) fd off len user_data =
  with_id t (fun id -> Uring.submit_readv_fixed t.uring fd id t.fixed_iobuf off len file_offset) user_data

let write t ?(file_offset=Int63.zero) fd off len user_data =
  with_id t (fun id -> Uring.submit_writev_fixed t.uring fd id t.fixed_iobuf off len file_offset) user_data

let writev t ?(offset=Int63.zero) fd iovec user_data =
  with_id t (fun id -> Uring.submit_writev t.uring fd id iovec offset) user_data

let poll_add t fd poll_mask user_data =
  with_id t (fun id -> Uring.submit_poll_add t.uring fd id poll_mask) user_data

let close t fd user_data =
  with_id t (fun id -> Uring.submit_close t.uring fd id) user_data

let submit t =
  if t.dirty then begin
    t.dirty <- false;
    Uring.submit t.uring
  end else
    0

type 'a completion_option =
  | None
  | Some of { result: int; data: 'a }

(* TODO use unixsupport.h *)
let errno_is_retry = function -62 | -11 | -4 -> true |_ -> false

let fn_on_ring fn t =
   let (id : Heap.ptr), res = fn t.uring in
   match (id :> int), res with
   | -1, res when errno_is_retry res ->
     None
   | -1, res when res < 0 ->
     failwith ("wait error " ^ (string_of_int res))
     (* TODO switch to unixsupport.h to raise Unix_error *)
   | _, res ->
     let data = Heap.free t.user_data id in
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
