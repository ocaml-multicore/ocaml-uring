type t

external ring_setup : int -> t = "ring_setup"
external ring_queue_write : t -> Unix.file_descr -> (Bigstringaf.t -> int -> unit) -> Bigstringaf.t -> int -> unit = "ring_queue_write"
external ring_queue_read : t -> Unix.file_descr -> (Bigstringaf.t -> int -> unit) -> Bigstringaf.t -> int -> unit = "ring_queue_read"
external ring_queue_accept : t -> Unix.file_descr -> (Unix.file_descr -> unit) -> unit = "ring_queue_accept"
external ring_queue_close : t -> Unix.file_descr -> unit = "ring_queue_close"
external ring_submit : t -> int = "ring_submit"
external ring_exit : t -> unit = "ring_exit"
external ring_wait : t -> unit = "ring_wait"