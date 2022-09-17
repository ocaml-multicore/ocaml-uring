(*
 * Copyright (c) 2021 Craig Ferguson <me@craigfe.io>
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

type 'a t
(** A bounded heap of values of type ['a]. *)

val create : int -> _ t
(** [create n] is a heap that holds at most [n] elements. *)

type 'a entry
(** An element in a heap. *)

type ptr = private int
(** The index of an entry. *)

val ptr : 'a entry -> ptr
(** [ptr e] is the index of [e].
    @raise Invalid_arg if [e] has already been freed. *)

val alloc : 'a t -> 'a -> extra_data:'b -> 'a entry
(** [alloc t a ~extra_data] adds the value [a] to [t] and returns a
    pointer to that value, or raises {!Invalid_arg} if no extra space
    can be created for [t], or [t] has already been [release]d.
    @param extra_data Prevent this from being GC'd until [free] is called. *)

val free : 'a t -> ptr -> 'a
(** [free t p] returns the element referenced by [p] and removes it from the
    heap. Has undefined behaviour if [p] has already been freed. *)

val in_use : 'a t -> int
(** [in_use t] is the number of entries currently allocated. *)

val release : _ t -> unit
(** [release t] marks [t] as unusable. Future operations on it will fail. [t] must be idle. *)

val is_released : _ t -> bool
(** [is_released t] is [true] once {!release} has succeeded. *)
