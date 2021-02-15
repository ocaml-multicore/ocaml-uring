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

module Iovec = Iovec

type uring
external uring_create : int -> uring = "ocaml_uring_setup"
external uring_exit : uring -> unit = "ocaml_uring_exit"

external uring_unregister_bigarray : uring -> unit = "ocaml_uring_unregister_ba"
external uring_register_bigarray : uring ->  Iovec.buf -> unit = "ocaml_uring_register_ba"
external uring_submit : uring -> int = "ocaml_uring_submit"

type id = int
external uring_submit_readv : uring -> Unix.file_descr -> id -> Iovec.t -> int -> unit = "ocaml_uring_submit_readv"
external uring_submit_writev : uring -> Unix.file_descr -> id -> Iovec.t -> int -> unit = "ocaml_uring_submit_writev"

external uring_submit_readv_fixed : uring -> Unix.file_descr -> id -> Iovec.buf -> int -> int -> int -> unit = "ocaml_uring_submit_readv_fixed_byte" "ocaml_uring_submit_readv_fixed_native"
external uring_submit_writev_fixed : uring -> Unix.file_descr -> id -> Iovec.buf -> int -> int -> int -> unit = "ocaml_uring_submit_writev_fixed_byte" "ocaml_uring_submit_writev_fixed_native"

external uring_wait_cqe : uring -> id * int = "ocaml_uring_wait_cqe"
external uring_peek_cqe : uring -> id * int = "ocaml_uring_peek_cqe"


type 'a t = {
  uring: uring;
  mutable fixed_iobuf: Iovec.buf;
  mutable id_freelist: int list;
  user_data: 'a array;
  queue_depth: int;
  mutable dirty: bool; (* has outstanding requests that need to be submitted *)
}

let default_iobuf_len = 1024 * 1024 (* 1MB *)

let create ?(fixed_buf_len=default_iobuf_len) ~queue_depth ~default () =
  let uring = uring_create queue_depth in
  (* TODO posix memalign this to page *)
  let fixed_iobuf = Iovec.alloc_buf fixed_buf_len in
  uring_register_bigarray uring fixed_iobuf; 
  Gc.finalise uring_exit uring;
  let id_freelist = List.init queue_depth (fun i -> i) in
  let user_data = Array.init queue_depth (fun _ -> default) in
  { uring; fixed_iobuf; id_freelist; user_data; dirty=false; queue_depth }

let realloc t iobuf =
  uring_unregister_bigarray t.uring;
  t.fixed_iobuf <- iobuf;
  uring_register_bigarray t.uring iobuf

let exit {uring;_} = uring_exit uring

let get_id t =
  match t.id_freelist with
  | [] -> raise Not_found
  | hd::tl -> t.id_freelist <- tl; hd

let put_id t v =
  t.id_freelist <- v :: t.id_freelist

let readv t ?(offset=0) fd iovec user_data =
  let id = get_id t in
  uring_submit_readv t.uring fd id iovec offset;
  t.dirty <- true;
  t.user_data.(id) <- user_data

let read t ?(file_offset=0) fd off len user_data =
  let id = get_id t in
  uring_submit_readv_fixed t.uring fd id t.fixed_iobuf off len file_offset;
  t.dirty <- true;
  t.user_data.(id) <- user_data

let write t ?(file_offset=0) fd off len user_data =
  let id = get_id t in
  uring_submit_writev_fixed t.uring fd id t.fixed_iobuf off len file_offset;
  t.dirty <- true;
  t.user_data.(id) <- user_data

let writev t ?(offset=0) fd iovec user_data =
  let id = get_id t in
  uring_submit_writev t.uring fd id iovec offset;
  t.dirty <- true;
  t.user_data.(id) <- user_data

let submit t =
  if t.dirty then begin
    t.dirty <- false;
    uring_submit t.uring
  end else
    0

(* TODO use unixsupport.h *)
let errno_is_retry = function -11 | -4 -> true |_ -> false

let fn_on_ring fn t =
   let id, res = fn t.uring in
   match id, res with
   | -1, res when errno_is_retry res ->
     None
   | -1, res when res < 0 ->
     failwith ("wait error " ^ (string_of_int res))
     (* TODO switch to unixsupport.h to raise Unix_error *)
   | id, res ->
     let data = t.user_data.(id) in
     put_id t id;
     Some (data, res)

let peek t = fn_on_ring uring_peek_cqe t
let wait t = fn_on_ring uring_wait_cqe t
let queue_depth {queue_depth;_} = queue_depth
let buf {fixed_iobuf;_} = fixed_iobuf
