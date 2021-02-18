(*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Iovec = Iovec

type 'a t

val create : ?fixed_buf_len:int -> queue_depth:int -> default:'a -> unit -> 'a t
val queue_depth : 'a t -> int
val exit : 'a t -> unit

val readv : 'a t -> ?offset:int -> Unix.file_descr -> Iovec.t -> 'a -> unit
val writev : 'a t -> ?offset:int -> Unix.file_descr -> Iovec.t -> 'a -> unit

val read : 'a t -> ?file_offset:int -> Unix.file_descr -> int -> int -> 'a -> unit

val write : 'a t -> ?file_offset:int -> Unix.file_descr -> int -> int -> 'a -> unit

val submit : 'a t -> int

val wait : 'a t -> ('a * int) option
val peek : 'a t -> ('a * int) option

val realloc : 'a t -> Iovec.buf -> unit
val buf : 'a t -> Iovec.buf
