type t
type chunk
exception No_space

val init: blocksize:int -> Bigstringaf.t -> int -> t

val alloc : t -> chunk

val free : t -> chunk -> unit

val to_offset : chunk -> int

val to_bigstring : t -> chunk -> Bigstringaf.t

val to_string : t -> chunk -> string
