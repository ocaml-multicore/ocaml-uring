type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type iovec
type t = iovec * buf array
external alloc_iovec : buf array -> iovec = "ocaml_uring_alloc_iovecs"
external free_iovec : iovec -> unit = "ocaml_uring_free_iovecs"
external adjust_iovec : iovec -> int -> int -> unit = "ocaml_iovec_advance_offset"

let alloc_buf len =
  Bigarray.(Array1.create char c_layout len)

let alloc bufs : t =
  let v = alloc_iovec bufs in
  v, bufs

let advance (iovec,_) ~idx ~adj =
  adjust_iovec iovec idx adj

let free (iov,_) = free_iovec iov

let nr_vecs (_,bufs) = Array.length bufs

let bufs t = snd t

let empty = alloc [||]
