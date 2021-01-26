type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type iovec
type t

val alloc : buf array -> t
val alloc_buf : int -> buf
val free : t -> unit
val nr_vecs : t -> int
val bufs : t -> buf array
val empty : t
