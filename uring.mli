module Iovec = Iovec

type 'a t

val create : queue_depth:int -> default:'a -> unit -> 'a t

val submit_readv : 'a t -> Unix.file_descr -> Iovec.t -> 'a -> unit
val submit_writev : 'a t -> Unix.file_descr -> Iovec.t -> 'a -> unit
val submit : 'a t -> int

val wait : 'a t -> 'a * int
