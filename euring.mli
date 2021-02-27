type t

val fork : (unit -> unit) -> unit

val yield : unit -> unit

val sleep : float -> unit

val read : ?file_offset:int -> Unix.file_descr -> int -> Baregion.chunk * int

val write : ?file_offset:int -> Unix.file_descr -> int -> Baregion.chunk * int

val run : (unit -> unit) -> unit
