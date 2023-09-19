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

module Config = Uring_config

module Bstruct : sig
  type t

  val length : t -> int

  val to_string : ?off:int -> ?len:int -> t -> string

  val shiftv : t list -> int -> t list
end

module Slab : sig
  type t

  val create : int -> t
  (** [create size] is a new slab. *)

  val slice : t -> int -> Bstruct.t
  (** [slice slab len] returns a new bstruct from the bigger slab if there
      is space. *)

  val slice_string : t -> string -> Bstruct.t

  val slice_strings : t -> string list -> Bstruct.t list
  (** [slice_strings t s] will copy the strings into the slab and return bstructs for them. *)
end

(** Io_uring interface. *)

module Region = Region

type 'a t
(** ['a t] is a reference to an Io_uring structure. *)

type 'a job
(** A handle for a submitted job, which can be used to cancel it.
    If an operation returns [None], this means that submission failed because the ring is full. *)

val major_alloc_byte_size : int

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

val set_fixed_buffer : 'a t -> bytes -> (unit, [> `ENOMEM]) result
(** [set_fixed_buffer t buf] sets [buf] as the fixed buffer for [t].

    You will normally want to wrap this with {!Region.alloc} or similar
    to divide the buffer into chunks.

    If [t] already has a buffer set, the old one will be removed.

    Returns [`ENOMEM] if insufficient kernel resources are available
    or the caller's RLIMIT_MEMLOCK resource limit would be exceeded.

    @raise Invalid_argument if there are any requests in progress *)

val buf : 'a t -> bytes
(** [buf t] is the fixed internal memory buffer associated with uring [t]
    using {!set_fixed_buffer}, or a zero-length buffer if none is set. *)

(** {2 Queueing operations} *)

val noop : 'a t -> 'a -> 'a job option
(** [noop t d] submits a no-op operation to uring [t]. The user data [d] will be
    returned by {!wait} or {!peek} upon completion. *)

(** {2 Timeout} *)

type clock = Boottime | Realtime
(** Represents Linux clocks. [Boottime] and [Realtime] represents OS clocks CLOCK_BOOTTIME
    and CLOCK_REALTIME respectively. *)

val timeout: ?absolute:bool -> 'a t -> clock -> int64 -> 'a -> 'a job option
(** [timeout t clock ns d] submits a timeout request to uring [t].

    [absolute] denotes how [clock] and [ns] relate to one another. Default value is [false]

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

val unlink : 'a t -> dir:bool -> ?fd:Unix.file_descr -> string -> 'a -> 'a job option
(** [unlink t ~dir ~fd path] removes the directory entry [path], which is resolved relative to [fd].
    If [fd] is not given, then the current working directory is used.
    If [path] is a symlink, the link is removed, not the target.
    @param dir If [true], this acts like [rmdir] (only removing empty directories).
               If [false], it acts like [unlink] (only removing non-directories). *)

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

val read : 'a t -> file_offset:offset -> Unix.file_descr -> Bstruct.t -> 'a -> 'a job option
(** [read t ~file_offset fd buf d] will submit a [read(2)] request to uring [t].
    It reads from absolute [file_offset] on the [fd] file descriptor and writes
    the results into the memory pointed to by [buf].  The user data [d] will
    be returned by {!wait} or {!peek} upon completion. *)

val write : 'a t -> file_offset:offset -> Unix.file_descr -> Bstruct.t -> 'a -> 'a job option
(** [write t ~file_offset fd buf d] will submit a [write(2)] request to uring [t].
    It writes to absolute [file_offset] on the [fd] file descriptor from the
    the memory pointed to by [buf].  The user data [d] will be returned by
    {!wait} or {!peek} upon completion. *)

val iov_max : int
(** The maximum length of the list that can be passed to [readv] and similar. *)

val readv : 'a t -> file_offset:offset -> Unix.file_descr -> Bstruct.t list -> 'a -> 'a job option
(** [readv t ~file_offset fd iov d] will submit a [readv(2)] request to uring [t].
    It reads from absolute [file_offset] on the [fd] file descriptor and writes
    the results into the memory pointed to by [iov].  The user data [d] will
    be returned by {!wait} or {!peek} upon completion.

    Requires [List.length iov <= Uring.iov_max] *)

val writev : 'a t -> file_offset:offset -> Unix.file_descr -> Bstruct.t list -> 'a -> 'a job option
(** [writev t ~file_offset fd iov d] will submit a [writev(2)] request to uring [t].
    It writes to absolute [file_offset] on the [fd] file descriptor from the
    the memory pointed to by [iov].  The user data [d] will be returned by
    {!wait} or {!peek} upon completion.

    Requires [List.length iov <= Uring.iov_max] *)

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

module Statx : sig
  type t
  (** A statx struct. *)

  type kind = [
    | `Unknown
    | `Fifo
    | `Character_special
    | `Directory
    | `Block_device
    | `Regular_file
    | `Symbolic_link
    | `Socket
  ]

  val pp_kind : kind Fmt.t

  val create : unit -> t
  (** Use [create] to make a statx result buffer to pass to {! statx}. *)

  module Flags : sig
    include FLAGS
    
    val empty : t
    val empty_path : t
    val no_automount : t
    val symlink_nofollow : t
    val statx_sync_as_stat : t
    val statx_force_sync : t
    val statx_dont_sync : t
  end

  module Attr : sig
    include FLAGS

    val compressed : t
    val immutable : t
    val append : t
    val nodump : t
    val encrypted : t
    val verity : t

    val dax : t
    (** Since Linux 5.8 *)

    val check : ?mask:Int64.t -> Int64.t -> t -> bool
    (** [check ?mask attr t] will check if [t] is set in [attr].

        If [mask] is not [None] then it will first check the mask to see
        if the file attribute is supported and if not raise [Invalid_argument]. *)
  end

  module Mask : sig
    include FLAGS

    val type' : t
    val mode : t
    val nlink : t
    val uid : t
    val gid : t
    val atime : t
    val mtime : t
    val ctime : t
    val ino : t
    val size : t
    val blocks : t
    val basic_stats : t (** All of the above flags. *)

    val btime : t

    val mnt_id : t
    (** As of Linux 5.8 *)

    val dioalign : t
    (** As of Linux 6.1 *)

    val check : Int64.t -> t -> bool
    (** [check mask t] checks if [t] is set in [mask]. *)
  end

  (** You may wish to use {! Mask.check} to verify the field has actually
      been returned with a sensible value first. *)

  val blksize : t -> Int64.t
  val attributes : t -> Int64.t
  val nlink : t -> Int64.t
  val uid : t -> Int64.t
  val gid : t -> Int64.t
  val ino : t -> Int64.t
  val size : t -> Int64.t
  val blocks : t -> Int64.t
  val attributes_mask : t -> Int64.t
  val rdev : t -> Int64.t
  val dev : t -> Int64.t
  val mask : t -> Int64.t
  
  val mnt_id : t -> Int64.t
  (** See {! Mask.mnt_id}. *)

  val dio_mem_align : t -> Int64.t
  (** See {! Mask.dioalign}. *)

  val dio_offset_align : t -> Int64.t
  (** See {! Mask.dioalign}. *)

  val atime_sec : t -> int64
  val btime_sec : t -> int64
  val ctime_sec : t -> int64
  val mtime_sec : t -> int64
  
  val atime_nsec : t -> int
  val btime_nsec : t -> int
  val ctime_nsec : t -> int
  val mtime_nsec : t -> int
  
  val mode : t -> int
  val perm : t -> int

  val kind : t -> kind
end

val statx : 'a t -> ?fd:Unix.file_descr -> mask:Statx.Mask.t -> string -> Statx.t -> Statx.Flags.t -> 'a -> 'a job option
(** [statx t ?fd ~mask path stat flags] stats [path], which is resolved relative to [fd]
    (or the current directory if [fd] is not given). *)

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

  val create : ?n_fds:int -> ?addr:Sockaddr.t -> Bstruct.t list -> t
  (** [create buffs] makes a new [msghdr] using the [buffs]
      for the underlying [iovec].

      Requires [List.length buffs <= Uring.iov_max]

      @param addr The remote address.
                  Use {!Sockaddr.create} to create a dummy address that will be filled when data is received.
      @param n_fds Reserve space to receive this many FDs (default 0) *)

  val get_fds : t -> Unix.file_descr list
end

val send_msg : ?fds:Unix.file_descr list -> ?dst:Unix.sockaddr -> 'a t -> Unix.file_descr -> Bstruct.t list -> 'a -> 'a job option
(** [send_msg t fd buffs d] will submit a [sendmsg(2)] request. The [Msghdr] will be constructed
    from the FDs ([fds]), address ([dst]) and buffers ([buffs]).

    Requires [List.length buffs <= Uring.iov_max]

    @param dst Destination address.
    @param fds Extra file descriptors to attach to the message. *)

val recv_msg : 'a t -> Unix.file_descr -> Msghdr.t -> 'a -> 'a job option
(** [recv_msg t fd msghdr d] will submit a [recvmsg(2)] request. If the request is
    successful then the [msghdr] will contain the sender address and the data received. *)

(** {2 Probing}

    You can check which operations are supported by the running kernel. *)

module Op = Config.Op

type probe

val get_probe : _ t -> probe
val op_supported : probe -> Op.t -> bool

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
    seconds for any outstanding events to complete on uring [t].
    This calls {!submit} automatically. *)

val get_cqe_nonblocking : 'a t -> 'a completion_option
(** [get_cqe_nonblocking t] returns the next completion entry from the uring [t].
    It is like {!wait} except that it returns [None] instead of blocking. *)

val peek : 'a t -> 'a completion_option
[@@deprecated "Renamed to Uring.get_cqe_nonblocking"]

val register_eventfd : 'a t -> Unix.file_descr -> unit
(** [register_eventfd t fd] will register an eventfd to the the uring [t].
    See documentation for io_uring_register_eventfd *)

val error_of_errno : int -> Unix.error
(** [error_of_errno e] converts the error code [abs e] to a Unix error type. *)

val active_ops : _ t -> int
(** [active_ops t] returns the number of operations added to the ring (whether submitted or not)
    for which the completion event has not yet been collected. *)

module Stats : sig
  type t = {
    sqe_ready : int;            (** SQEs not yet submitted. *)
    active_ops : int;           (** See {!Uring.active_ops}. *)
    sketch_buffer_size : int;   (** Size of the current sketch buffer. *)
    sketch_used : int;          (** Bytes used within current sketch buffer. *)
    sketch_old_buffers : int;   (** Old sketch buffers waiting to be freed. *)
  }

  val pp : t Fmt.t
end

val get_debug_stats : _ t -> Stats.t
(** [get_debug_stats t] collects some metrics about the internal state of [t]. *)

module Private : sig
  module Heap = Heap
end

module Bytes : sig
  val shiftv : bytes list -> int -> bytes list
  val of_bigarray : off:int -> len:int -> (char, 'a, 'b) Bigarray.Array1.t -> bytes
end
