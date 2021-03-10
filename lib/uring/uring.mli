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

(** Io_uring interface.
    FIXME: Since this library is still unreleased, all the interfaces here are
    being iterated on.
*)

module Region = Region

type 'a t
(** ['a t] is a reference to an Io_uring structure. *)

val create : ?fixed_buf_len:int -> queue_depth:int -> default:'a -> unit -> 'a t
(** [create ?fixed_buf_len ~queue_depth ~default] will return a fresh
    Io_uring structure [t].  Each [t] has associated with it a fixed region of
    memory that is used for the "fixed buffer" mode of io_uring to avoid data
    copying between userspace and the kernel. *)

val queue_depth : 'a t -> int
(** [queue_depth t] returns the total number of submission slots for the uring [t] *)

val exit : 'a t -> unit
(** [exit t] will shut down the uring [t]. Any subsequent requests will fail. *)

val readv : 'a t -> ?offset:int -> Unix.file_descr -> Iovec.t -> 'a -> bool
(** [readv t ?offset fd iov d] will submit a [readv(2)] request to uring [t].
    It reads from absolute file [offset] on the [fd] file descriptor and writes
    the results into the memory pointed to by [iov].  The user data [d] will
    be returned by {!wait} or {!peek} upon completion. *)

val writev : 'a t -> ?offset:int -> Unix.file_descr -> Iovec.t -> 'a -> bool
(** [writev t ?offset fd iov d] will submit a [writev(2)] request to uring [t].
    It writes to absolute file [offset] on the [fd] file descriptor from the
    the memory pointed to by [iov].  The user data [d] will be returned by
    {!wait} or {!peek} upon completion. *)

val read : 'a t -> ?file_offset:int -> Unix.file_descr -> int -> int -> 'a -> bool
(** [read t ?file_offset fd off d] will submit a [read(2)] request to uring [t].
    It read from absolute [file_offset] on the [fd] file descriptor and writes
    the results into the fixed memory buffer associated with uring [t] at offset
    [off]. TODO: replace [off] with {!Region.chunk} instead?
    The user data [d] will be returned by {!wait} or {!peek} upon completion. *)

val write : 'a t -> ?file_offset:int -> Unix.file_descr -> int -> int -> 'a -> bool
(** [write t ?file_offset fd off d] will submit a [write(2)] request to uring [t].
    It writes into absolute [file_offset] on the [fd] file descriptor from
    the fixed memory buffer associated with uring [t] at offset [off].
    TODO: replace [off] with {!Region.chunk} instead?
    The user data [d] will be returned by {!wait} or {!peek} upon completion. *)

val submit : 'a t -> int
(** [submit t] will submit all the outstanding queued requests on uring [t]
    to the kernel. Their results can subsequently be retrieved using {!wait}
    or {!peek}. *)

val wait : ?timeout:float -> 'a t -> ('a * int) option
(** [wait ?timeout t] will block indefinitely (the default) or for [timeout]
    seconds for any outstanding events to complete on uring [t].  Events should
    have been queued via {!submit} previously to this call.
    It returns the user data associated with the original request and the 
    integer syscall result. TODO: replace int res with a GADT of the request type. *)

val peek : 'a t -> ('a * int) option
(** [peek t] looks for completed requests on the uring [t] without blocking.
    It returns the user data associated with the original request and the 
    integer syscall result. TODO: replace int res with a GADT of the request type. *)

val realloc : 'a t -> Iovec.Buffer.t -> unit
(** [realloc t buf] will replace the internal fixed buffer associated with
    uring [t] with a fresh one. TODO: specify semantics of outstanding requests. *)

val buf : 'a t -> Iovec.Buffer.t
(** [buf t] will return the fixed internal memory buffer associated with
    uring [t]. TODO: replace with {!Region.t} instead. *)
