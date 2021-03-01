type t

val fork : (unit -> unit) -> unit

val yield : unit -> unit

val sleep : float -> unit

val alloc : unit -> Baregion.chunk

val free : Baregion.chunk -> unit

val read : ?file_offset:int -> Unix.file_descr -> Baregion.chunk -> int -> unit

val write : ?file_offset:int -> Unix.file_descr -> Baregion.chunk -> int -> unit

val run : ?queue_depth:int -> ?block_size:int -> (unit -> unit) -> unit
