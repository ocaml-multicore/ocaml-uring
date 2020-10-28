type t

external ring_setup : int -> t = "ring_setup"
external ring_queue_write : t -> Unix.file_descr -> (Bigstringaf.t -> int -> unit) -> int -> int -> unit = "ring_queue_write"
external ring_queue_read : t -> Unix.file_descr -> (Bigstringaf.t -> int -> unit) -> int -> int -> unit = "ring_queue_read"
external ring_queue_close : t -> Unix.file_descr -> unit = "ring_queue_close"
external ring_submit : t -> int = "ring_submit"
external ring_close : t -> unit = "ring_close"