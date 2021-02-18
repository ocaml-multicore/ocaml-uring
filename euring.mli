type t

val fork : (t -> unit) -> unit
val yield : unit -> unit

val read : t -> ?file_offset:int -> Unix.file_descr -> int -> Bigstringaf.t
val write : t -> ?file_offset:int -> Unix.file_descr -> int -> unit
val run : (t -> unit) -> unit
