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

module Buffer = struct
  type t =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let create len = Bigarray.(Array1.create char c_layout len)
end

module Iovec = struct
  type t

  external alloc : Buffer.t array -> t = "ocaml_iovec_alloc"
  external free : t -> unit = "ocaml_iovec_free"
  external adjust : t -> int -> int -> unit = "ocaml_iovec_advance"
end

type t = Iovec.t * Buffer.t array

let alloc bufs : t = (Iovec.alloc bufs, bufs)
let advance (iov, _) ~idx ~adj = Iovec.adjust iov idx adj
let free (iov, _) = Iovec.free iov
let length (_, bufs) = Array.length bufs
let buffers (_, bufs) = bufs
let empty = alloc [||]
