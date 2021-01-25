type uring

type iobuf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let iobuf_alloc len =   Bigarray.(Array1.create char c_layout len)

(** [ring_setup i] allocates a new io_uring with queue depth [i].
     @raise [Failure] *)
external uring_create : int -> uring = "ocaml_uring_setup"
external uring_exit : uring -> unit = "ocaml_uring_exit"
(* external uring_register_bigarray : uring ->  iobuf -> unit = "ocaml_uring_register_ba" *)
external uring_submit : uring -> int = "ocaml_uring_submit"

type iovecs
external uring_alloc_iovecs : iobuf array -> iovecs = "ocaml_uring_alloc_iovecs"
external uring_free_iovecs : iovecs -> unit = "ocaml_uring_free_iovecs"

external uring_submit_readv : uring -> Unix.file_descr -> iovecs -> int -> unit = "ocaml_uring_submit_readv"
external uring_wait_cqe : uring -> iovecs * int = "ocaml_uring_wait_cqe"

type t = {
  uring: uring;
  iobuf: iobuf;
  pending: (iovecs, iobuf array) Hashtbl.t;
}

let default_iobuf_len = 1024 * 1024 (* 1MB *)

let create ~queue_depth () =
  let uring = uring_create queue_depth in
  (* TODO posix memalign this to page *)
  let iobuf = Bigarray.(Array1.create char c_layout default_iobuf_len) in
  (* uring_register_bigarray uring iobuf; *)
  Gc.finalise uring_exit uring;
  let pending = Hashtbl.create 1 in
  { uring; iobuf; pending }

let submit_readv {uring;pending;_} fd bufs =
  let len = Array.length bufs in
  let iovs = uring_alloc_iovecs bufs in
  uring_submit_readv uring fd iovs len;
  Hashtbl.add pending iovs bufs;
  ()

let submit {uring;_} =
  uring_submit uring

let wait_cqe {uring;pending;_} =
   let iovecs, len = uring_wait_cqe uring in
   let bas = Hashtbl.find pending iovecs in
   uring_free_iovecs iovecs;
   bas, len


(*
external ring_queue_write_full : t -> Unix.file_descr -> (Bigstringaf.t -> int -> unit) -> Bigstringaf.t -> int -> unit = "ring_queue_write_full"
external ring_queue_read : t -> Unix.file_descr -> (Bigstringaf.t -> int -> unit) -> Bigstringaf.t -> int -> unit = "ring_queue_read"
external ring_queue_accept : t -> Unix.file_descr -> (Unix.file_descr -> unit) -> unit = "ring_queue_accept"
external ring_queue_close : t -> Unix.file_descr -> unit = "ring_queue_close"
external ring_submit : t -> int = "ring_submit"
external ring_exit : t -> unit = "ring_exit"
external ring_wait : t -> unit = "ring_wait"
*) 