module Iovec = Iovec

type 'a t

val create : queue_depth:int -> default:'a -> unit -> 'a t

val readv : 'a t -> ?offset:int -> Unix.file_descr -> Iovec.t -> 'a -> unit
val writev : 'a t -> ?offset:int -> Unix.file_descr -> Iovec.t -> 'a -> unit
val submit : 'a t -> int

val wait : 'a t -> 'a * int
