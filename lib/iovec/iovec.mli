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

(** [Buffer.t] is an OCaml Bigarray.
    This may move to a more efficient structure in the future without reference counting. *)
module Buffer : sig
  type t =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  val create : int -> t
  (** [create l] creates an iovec with a single malloced block of memory of length [l] *)
end

type t
(** [t] represents the [iovec] and the backing [buf] list which the [iovec] points at. *)

val empty : t
(** [empty] is an iovec pointing to no memory buffers. *)

val alloc : Buffer.t array -> t
(** [alloc bufs] create an iovec from the array of memory buffers passed in. *)

val free : t -> unit
(** [free t] will free the memory buffers pointed to by [t]. The memory buffers will be
    NULL after this call. *)

val length : t -> int
(** [length t] will return the number of memory buffers tracked by the iovec. *)

val buffers : t -> Buffer.t array
(** [buffers t] returns the underlying memory buffers tracked by the iovec. *)

val advance : t -> idx:int -> adj:int -> unit
(** FIXME [advance] is a way to adjust the iovec, but an unfinished and unsafe interface. *)
