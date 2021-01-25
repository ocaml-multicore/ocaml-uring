
type t
type iobuf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val iobuf_alloc : int -> iobuf
val create : queue_depth:int -> unit -> t
val submit_readv : t -> Unix.file_descr -> iobuf array -> unit
val submit : t -> int
val wait_cqe : t -> iobuf array * int