module Iovec = Iovec

type uring
external uring_create : int -> uring = "ocaml_uring_setup"
external uring_exit : uring -> unit = "ocaml_uring_exit"
(* external uring_register_bigarray : uring ->  iobuf -> unit = "ocaml_uring_register_ba" *)
external uring_submit : uring -> int = "ocaml_uring_submit"

type id = int
external uring_submit_readv : uring -> Unix.file_descr -> id -> Iovec.t -> int -> unit = "ocaml_uring_submit_readv"
external uring_submit_writev : uring -> Unix.file_descr -> id -> Iovec.t -> int -> unit = "ocaml_uring_submit_writev"

external uring_wait_cqe : uring -> id * int = "ocaml_uring_wait_cqe"

type 'a t = {
  uring: uring;
  iobuf: Iovec.buf;
  mutable id_freelist: int list;
  user_data: 'a array;
}

let default_iobuf_len = 1024 * 1024 (* 1MB *)

let create ~queue_depth ~default () =
  let uring = uring_create queue_depth in
  (* TODO posix memalign this to page *)
  let iobuf = Iovec.alloc_buf default_iobuf_len in
  (* uring_register_bigarray uring iobuf; *)
  Gc.finalise uring_exit uring;
  let id_freelist = List.init queue_depth (fun i -> i) in
  let user_data = Array.init queue_depth (fun _ -> default) in
  { uring; iobuf; id_freelist; user_data }

let get_id t =
  match t.id_freelist with
  | [] -> raise Not_found
  | hd::tl -> t.id_freelist <- tl; hd

let put_id t v =
  t.id_freelist <- v :: t.id_freelist

let readv t ?(offset=0) fd iovec user_data =
  let id = get_id t in
  uring_submit_readv t.uring fd id iovec offset;
  t.user_data.(id) <- user_data

let writev t ?(offset=0) fd iovec user_data =
  let id = get_id t in
  uring_submit_writev t.uring fd id iovec offset;
  t.user_data.(id) <- user_data

let submit {uring;_} =
  uring_submit uring

let wait t =
   let id, res = uring_wait_cqe t.uring in
   let data = t.user_data.(id) in
   put_id t id;
   data, res

(*
external ring_queue_write_full : t -> Unix.file_descr -> (Bigstringaf.t -> int -> unit) -> Bigstringaf.t -> int -> unit = "ring_queue_write_full"
external ring_queue_read : t -> Unix.file_descr -> (Bigstringaf.t -> int -> unit) -> Bigstringaf.t -> int -> unit = "ring_queue_read"
external ring_queue_accept : t -> Unix.file_descr -> (Unix.file_descr -> unit) -> unit = "ring_queue_accept"
external ring_queue_close : t -> Unix.file_descr -> unit = "ring_queue_close"
external ring_submit : t -> int = "ring_submit"
external ring_exit : t -> unit = "ring_exit"
external ring_wait : t -> unit = "ring_wait"
*) 
