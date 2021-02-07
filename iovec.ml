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

type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type iovec
type t = iovec * buf array
external alloc_iovec : buf array -> iovec = "ocaml_uring_alloc_iovecs"
external free_iovec : iovec -> unit = "ocaml_uring_free_iovecs"
external adjust_iovec : iovec -> int -> int -> unit = "ocaml_iovec_advance_offset"

let alloc_buf len =
  Bigarray.(Array1.create char c_layout len)

let alloc bufs : t =
  let v = alloc_iovec bufs in
  v, bufs

let advance (iovec,_) ~idx ~adj =
  adjust_iovec iovec idx adj

let free (iov,_) = free_iovec iov

let nr_vecs (_,bufs) = Array.length bufs

let bufs t = snd t

let empty = alloc [||]
