type t
type chunk
exception No_space

val init: block_size:int -> Bigstringaf.t -> int -> t

val alloc : t -> chunk

val free : chunk -> unit

val to_offset : chunk -> int

val to_bigstring : ?len:int -> chunk -> Bigstringaf.t

val to_string : ?len:int -> chunk -> string

val avail : t -> int
