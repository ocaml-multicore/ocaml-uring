type t

val ring_setup : int -> t
val ring_queue_write : t -> Unix.file_descr -> (Bigstringaf.t -> int -> unit) -> int -> int -> unit
val ring_queue_read : t -> Unix.file_descr -> (Bigstringaf.t -> int -> unit) -> int -> int -> unit
val ring_queue_accept : t -> Unix.file_descr -> (Unix.file_descr -> unit) -> unit
val ring_queue_close : t -> Unix.file_descr -> unit
val ring_submit : t -> int
val ring_exit : t -> unit
val ring_wait : t -> unit