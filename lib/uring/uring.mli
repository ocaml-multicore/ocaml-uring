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

(** Io_uring interface. *)

module Region = Region

type 'a t
(** ['a t] is a reference to an Io_uring structure. *)

type 'a job
(** A handle for a submitted job, which can be used to cancel it.
    If an operation returns [None], this means that submission failed because the ring is full. *)

val create : ?polling_timeout:int -> queue_depth:int -> unit -> 'a t
(** [create ~queue_depth] will return a fresh Io_uring structure [t].
    Initially, [t] has no fixed buffer. Use {!set_fixed_buffer} if you want one.
    @param polling_timeout If given, use polling mode with the given idle timeout (in ms).
                           This requires privileges. *)

val queue_depth : 'a t -> int
(** [queue_depth t] returns the total number of submission slots for the uring [t] *)

val exit : 'a t -> unit
(** [exit t] will shut down the uring [t]. Any subsequent requests will fail.
    @raise Invalid_argument if there are any requests in progress *)

(** {2 Fixed buffers}

    Each uring may have associated with it a fixed region of memory that is used
    for the "fixed buffer" mode of io_uring to avoid data copying between
    userspace and the kernel. *)

val set_fixed_buffer : 'a t -> Cstruct.buffer -> (unit, [> `ENOMEM]) result
(** [set_fixed_buffer t buf] sets [buf] as the fixed buffer for [t].

    You will normally want to wrap this with {!Region.alloc} or similar
    to divide the buffer into chunks.

    If [t] already has a buffer set, the old one will be removed.

    Returns [`ENOMEM] if insufficient kernel resources are available
    or the caller's RLIMIT_MEMLOCK resource limit would be exceeded.

    @raise Invalid_argument if there are any requests in progress *)

val buf : 'a t -> Cstruct.buffer
(** [buf t] is the fixed internal memory buffer associated with uring [t]
    using {!set_fixed_buffer}, or a zero-length buffer if none is set. *)

(** {2 Queueing operations} *)

val noop : 'a t -> 'a -> 'a job option
(** [noop t d] submits a no-op operation to uring [t]. The user data [d] will be
    returned by {!wait} or {!peek} upon completion. *)

val timeout: ?rel:[`Relative | `Absolute] -> 'a t -> [`Boottime | `Realtime] -> int64 -> 'a -> 'a job option
(** [timeout t clock ns d] submits a timeout request to uring [t].

    [clock] [`Boottime] and [`Realtime] represents OS clocks CLOCK_BOOTTIME and CLOCK_REALTIME 
    respectively.

    [rel] denotes how [clock] and [ns] relates to one another. Default value is [`Relative]

    [ns] is the timeout time in nanoseconds *)

module type FLAGS = sig
  type t = private int
  (** A set of flags. *)

  val of_int : int -> t

  val ( + ) : t -> t -> t
  (** [a + b] is the union of the sets. *)

  val mem : t -> t -> bool
  (** [mem x flags] is [true] iff [x] is a subset of [flags]. *)
end

(** Flags that can be passed to openat2. *)
module Open_flags : sig
  include FLAGS

  val empty : t
  val append : t
  val cloexec : t
  val creat : t
  val direct : t
  val directory : t
  val dsync : t
  val excl : t
  val largefile : t
  val noatime : t
  val noctty : t
  val nofollow : t
  val nonblock : t
  val path : t
  val sync : t
  val tmpfile : t
  val trunc : t
end

(** Flags that can be passed to openat2 to control path resolution. *)
module Resolve : sig
  include FLAGS

  val empty : t
  val beneath : t
  val in_root : t
  val no_magiclinks : t
  val no_symlinks : t
  val no_xdev : t
  val cached : t
end

val openat2 : 'a t ->
  access:[`R|`W|`RW] ->
  flags:Open_flags.t ->
  perm:Unix.file_perm ->
  resolve:Resolve.t ->
  ?fd:Unix.file_descr ->
  string ->
  'a -> 'a job option
(** [openat2 t ~access ~flags ~perm ~resolve ~fd path d] opens [path], which is resolved relative to [fd]
    (or the current directory if [fd] is not given).
    The user data [d] will be returned by {!wait} or {!peek} upon completion.
    @param access controls whether the file is opened for reading, writing, or both
    @param flags are the usual open flags
    @param perm sets the access control bits for newly created files (subject to the process's umask)
    @param resolve controls how the pathname is resolved. *)

module Poll_mask : sig
  include FLAGS

  val pollin  : t
  val pollout : t
  val pollerr : t
  val pollhup : t
end

val poll_add : 'a t -> Unix.file_descr -> Poll_mask.t -> 'a -> 'a job option
(** [poll_add t fd mask d] will submit a [poll(2)] request to uring [t].
    It completes and returns [d] when an event in [mask] is ready on [fd]. *)

type offset := Optint.Int63.t
(** For files, give the absolute offset, or use [Optint.Int63.minus_one] for the current position.
    For sockets, use an offset of [Optint.Int63.zero] ([minus_one] is not allowed here). *)

val read : 'a t -> file_offset:offset -> Unix.file_descr -> Cstruct.t -> 'a -> 'a job option
(** [read t ~file_offset fd buf d] will submit a [read(2)] request to uring [t].
    It reads from absolute [file_offset] on the [fd] file descriptor and writes
    the results into the memory pointed to by [buf].  The user data [d] will
    be returned by {!wait} or {!peek} upon completion. *)

val write : 'a t -> file_offset:offset -> Unix.file_descr -> Cstruct.t -> 'a -> 'a job option
(** [write t ~file_offset fd buf d] will submit a [write(2)] request to uring [t].
    It writes to absolute [file_offset] on the [fd] file descriptor from the
    the memory pointed to by [buf].  The user data [d] will be returned by
    {!wait} or {!peek} upon completion. *)

val readv : 'a t -> file_offset:offset -> Unix.file_descr -> Cstruct.t list -> 'a -> 'a job option
(** [readv t ~file_offset fd iov d] will submit a [readv(2)] request to uring [t].
    It reads from absolute [file_offset] on the [fd] file descriptor and writes
    the results into the memory pointed to by [iov].  The user data [d] will
    be returned by {!wait} or {!peek} upon completion. *)

val writev : 'a t -> file_offset:offset -> Unix.file_descr -> Cstruct.t list -> 'a -> 'a job option
(** [writev t ~file_offset fd iov d] will submit a [writev(2)] request to uring [t].
    It writes to absolute [file_offset] on the [fd] file descriptor from the
    the memory pointed to by [iov].  The user data [d] will be returned by
    {!wait} or {!peek} upon completion. *)

val read_fixed : 'a t -> file_offset:offset -> Unix.file_descr -> off:int -> len:int -> 'a -> 'a job option
(** [read t ~file_offset fd ~off ~len d] will submit a [read(2)] request to uring [t].
    It reads up to [len] bytes from absolute [file_offset] on the [fd] file descriptor and
    writes the results into the fixed memory buffer associated with uring [t] at offset [off].
    The user data [d] will be returned by {!wait} or {!peek} upon completion. *)

val read_chunk : ?len:int -> 'a t -> file_offset:offset -> Unix.file_descr -> Region.chunk -> 'a -> 'a job option
(** [read_chunk] is like [read_fixed], but gets the offset from [chunk].
    @param len Restrict the read to the first [len] bytes of [chunk]. *)

val write_fixed : 'a t -> file_offset:offset -> Unix.file_descr -> off:int -> len:int -> 'a -> 'a job option
(** [write t ~file_offset fd off d] will submit a [write(2)] request to uring [t].
    It writes up to [len] bytes into absolute [file_offset] on the [fd] file descriptor
    from the fixed memory buffer associated with uring [t] at offset [off].
    The user data [d] will be returned by {!wait} or {!peek} upon completion. *)

val write_chunk : ?len:int -> 'a t -> file_offset:offset -> Unix.file_descr -> Region.chunk -> 'a -> 'a job option
(** [write_chunk] is like [write_fixed], but gets the offset from [chunk].
    @param len Restrict the write to the first [len] bytes of [chunk]. *)

val splice : 'a t -> src:Unix.file_descr -> dst:Unix.file_descr -> len:int -> 'a -> 'a job option
(** [splice t ~src ~dst ~len d] will submit a request to copy [len] bytes from [src] to [dst].
    The operation returns the number of bytes transferred, or 0 for end-of-input.
    The result is [EINVAL] if the file descriptors don't support splicing. *)

val connect : 'a t -> Unix.file_descr -> Unix.sockaddr -> 'a -> 'a job option
(** [connect t fd addr d] will submit a request to connect [fd] to [addr]. *)

(** Holder for the peer's address in {!accept}. *)
module Sockaddr : sig
  type t

  val create : unit -> t
  val get : t -> Unix.sockaddr
end

val accept : 'a t -> Unix.file_descr -> Sockaddr.t -> 'a -> 'a job option
(** [accept t fd addr d] will submit a request to accept a new connection on [fd].
    The new FD will be configured with [SOCK_CLOEXEC].
    The remote address will be stored in [addr]. *)

val close : 'a t -> Unix.file_descr -> 'a -> 'a job option

val cancel : 'a t -> 'a job -> 'a -> 'a job option
(** [cancel t job d] submits a request to cancel [job].
    The cancel job itself returns 0 on success, or [ENOTFOUND]
    if [job] had already completed by the time the kernel processed the cancellation request.
    @raise Invalid_argument if the job has already been returned by e.g. {!wait}. *)

module Msghdr : sig
  type t

  val create : ?n_fds:int -> ?addr:Sockaddr.t -> Cstruct.t list -> t
  (** [create buffs] makes a new [msghdr] using the [buffs]
      for the underlying [iovec].
      @param addr The remote address.
                  Use {!Sockaddr.create} to create a dummy address that will be filled when data is received.
      @param n_fds Reserve space to receive this many FDs (default 0) *)

  val get_fds : t -> Unix.file_descr list
end

val send_msg : ?fds:Unix.file_descr list -> ?dst:Unix.sockaddr -> 'a t -> Unix.file_descr -> Cstruct.t list -> 'a -> 'a job option
(** [send_msg t fd buffs d] will submit a [sendmsg(2)] request. The [Msghdr] will be constructed
    from the FDs ([fds]), address ([dst]) and buffers ([buffs]).
    @param dst Destination address.
    @param fds Extra file descriptors to attach to the message. *)

val recv_msg : 'a t -> Unix.file_descr -> Msghdr.t -> 'a -> 'a job option
(** [recv_msg t fd msghdr d] will submit a [recvmsg(2)] request. If the request is
    successful then the [msghdr] will contain the sender address and the data received. *)

(** {2 Submitting operations} *)

val submit : 'a t -> int
(** [submit t] will submit all the outstanding queued requests on uring [t]
    to the kernel. Their results can subsequently be retrieved using {!wait}
    or {!peek}. *)

type 'a completion_option =
  | None
  | Some of { result: int; data: 'a } (**)
(** The type of results of calling {!wait} and {!peek}. [None] denotes that
    either there were no completions in the queue or an interrupt / timeout
    occurred. [Some] contains both the user data attached to the completed
    request and the integer syscall result. *)

val wait : ?timeout:float -> 'a t -> 'a completion_option
(** [wait ?timeout t] will block indefinitely (the default) or for [timeout]
    seconds for any outstanding events to complete on uring [t]. Events should
    have been queued via {!submit} previously to this call. *)

val peek : 'a t -> 'a completion_option
(** [peek t] looks for completed requests on the uring [t] without blocking. *)

val error_of_errno : int -> Unix.error
(** [error_of_errno e] converts the error code [abs e] to a Unix error type. *)

module Private : sig
  module Heap = Heap
end
