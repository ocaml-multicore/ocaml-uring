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

(** Io_uring is an asynchronous I/O API for Linux that uses ring buffers
    shared between the Linux kernel and userspace to provide an efficient
    mechanism to batch requests that can be handled asynchronously and in
    parallel.  This module provides an OCaml interface to io_uring that
    aims to provide a thin type-safe layer for use in higher-level interfaces.
    @see <https://unixism.net/loti/what_is_io_uring.html#what-is-io-uring> What is Io_uring? *)

module Region = Region

(** Type of flags that can be combined. *)
module type FLAGS = sig
  type t = private int
  (** A set of flags. *)

  val empty : t

  val of_int : int -> t

  val ( + ) : t -> t -> t
  (** [a + b] is the union of the sets. *)

  val mem : t -> t -> bool
  (** [mem x flags] is [true] iff [x] is a subset of [flags]. *)
end

(** Flags that can be passed to {!create}. *)
module Setup_flags : sig
  include FLAGS

  val iopoll : t
  (** io_context is polled *)

  val clamp : t
  (** Clamp SQ/CQ ring sizes *)

  val r_disabled : t
  (** Start with ring disabled *)

  val submit_all : t
  (** Continue submit on error *)

  val coop_taskrun : t
  (** Cooperative task running *)

  val taskrun_flag : t
  (** Get notified if task work is available *)

  val sqe128 : t
  (** SQEs are 128 byte *)

  val cqe32 : t
  (** CQEs are 32 byte *)

  val single_issuer : t
  (** Only one task is allowed to submit requests *)

  val defer_taskrun : t
  (** Defer running task work to get events *)
end

type 'a t
(** ['a t] is a reference to an Io_uring structure. *)

type 'a job
(** A handle for a submitted job, which can be used to cancel it.
    If an operation returns [None], this means that submission failed because the ring is full. *)

val create : ?flags:Setup_flags.t -> ?polling_timeout:int -> queue_depth:int -> unit -> 'a t
(** [create ~queue_depth] will return a fresh Io_uring structure [t].
    Initially, [t] has no fixed buffer. Use {!set_fixed_buffer} if you want one.

    The [queue_depth] determines the size of the submission queue (SQ) and completion
    queue (CQ) rings. The kernel may round this up to the next power of 2. The actual
    size allocated can be checked with {!queue_depth}.

    @param flags Setup flags to configure ring behavior (see {!Setup_flags})
    @param polling_timeout If given, use polling mode with the given idle timeout (in ms).
                           This requires elevated privileges and enables {!Setup_flags.iopoll}.
    @raise Unix.Unix_error if the io_uring_setup system call fails *)

val queue_depth : 'a t -> int
(** [queue_depth t] returns the total number of submission slots for the uring [t] *)

val exit : 'a t -> unit
(** [exit t] will shut down the uring [t]. Any subsequent requests will fail.

    This closes the io_uring file descriptor and unmaps the memory rings.
    After calling this, the ring cannot be used again.

    @raise Invalid_argument if there are any requests in progress. Check with
           {!active_ops} to ensure all operations have completed. *)

(** {2 Fixed buffers}

    Each uring may have associated with it a fixed region of memory that is used
    for the "fixed buffer" mode of io_uring to avoid data copying between
    userspace and the kernel. *)

val set_fixed_buffer : 'a t -> Cstruct.buffer -> (unit, [> `ENOMEM]) result
(** [set_fixed_buffer t buf] sets [buf] as the fixed buffer for [t].

    Fixed buffers allow zero-copy I/O operations using {!read_fixed} and {!write_fixed}.
    The kernel pins the buffer in memory, avoiding the need to map user pages for each I/O.
    You will normally want to wrap this with {!Region.alloc} or similar to divide the
    buffer into chunks.

    If [t] already has a buffer set, the old one will be removed.

    @return [Ok ()] on success, or [Error `ENOMEM] if:
            - Insufficient kernel resources are available
            - The caller's RLIMIT_MEMLOCK resource limit would be exceeded
            - The buffer is too large to pin in memory
    @raise Invalid_argument if there are any requests in progress *)

val buf : 'a t -> Cstruct.buffer
(** [buf t] is the fixed internal memory buffer associated with uring [t]
    using {!set_fixed_buffer}, or a zero-length buffer if none is set. *)

(** {2 Queueing operations} *)

val noop : 'a t -> 'a -> 'a job option
(** [noop t d] submits a no-op operation to uring [t]. The user data [d] will be
    returned by {!wait} or {!get_cqe_nonblocking} upon completion.

    This operation does nothing but can be useful for testing the ring, waking up
    a thread waiting on completions, or as a barrier when used with IO_LINK.
    The completion will have [result = 0] on success.

    @return [None] if the submission queue is full; otherwise [Some job] *)

(** {2 Timeout} *)

(** Represents different Linux clocks. *)
type clock =
   Boottime (** [CLOCK_BOOTTIME] is a suspend-aware monotonic clock *)
 | Realtime (** [CLOCK_REALTIME] is a wallclock time clock that may be affected by discontinuous jumps *)

val timeout: ?absolute:bool -> 'a t -> clock -> int64 -> 'a -> 'a job option
(** [timeout t clock ns d] submits a timeout request to uring [t].

    The timeout will trigger after the specified time has elapsed. When the timeout
    expires, the completion's [result] will be negative with an [ETIME] error indicating
    timeout.  The timeout can be cancelled using {!cancel} before it triggers.

    @param absolute If [false] (default), [ns] is relative to the current time.
                    If [true], [ns] is an absolute time value according to [clock]
    @param clock The clock source: {!Boottime} (suspend-aware) or {!Realtime} (wall-clock)
    @param ns The timeout duration in nanoseconds (relative) or absolute time
    @return [None] if the submission queue is full; otherwise [Some job] *)

(** Flags that can be passed to {!openat2}. *)
module Open_flags : sig
  include FLAGS

  val append : t
  (** [append] repositions the file offset to the end of the file before
      every write. This should be used with caution with io_uring.
      @see <https://github.com/axboe/liburing/issues/32#issuecomment-1313220682> GitHub axboe/liburing#32 *)

  val cloexec : t
  (** [cloexec] enables the close-on-exec flag for the new fd. *)

  val creat : t
  (** [creat] implies that if the pathname does not exist, it is
      created as a regular file. *)

  val direct : t
  (** [direct] disables the kernel buffer cache and performs IO
      directly to and from the userspace buffers. *)

  val directory : t
  (** [directory] causes the open operation to fail if the target
      is not a directory. *)

  val dsync : t
  (** [dsync] ensures that write operations on the file complete
      according to the requirements of synchronised IO data integrity
      completion. *)

  val excl : t
  (** [excl] is used alongside {!creat} to ensure that the file
      is created as a result of the {!openat2} call, and otherwise
      fails with a {!Unix.EEXIST} exception. The only exception where
      [excl] can be used without {!creat} is when attempting to open
      block devices. If the block device is otherwise mounted, then
      the open will fail with {!Unix.EBUSY}. *)

  val noatime : t
  (** [noatime] signals that the file access time should not be updated
      when the file is read from. See {!Statx.atime_nsec}. *)

  val noctty : t
  (** [noctty] ensures that if the path refers to a tty, it will not
      be assigned as the controlling terminal even if one is not present. *)

  val nofollow : t
  (** [nofollow] will cause the open to fail with {!Unix.ELOOP} if the
      basename of the path is a symbolic link. *)

  val nonblock : t
  (** [nonblock] will open the file in non-blocking mode. *)

  val path : t
  (** [path] will obtain a fd that can only be used to either indicate
      a location in a filesystem tree, or perform operations at the fd
      level. The file is not opened, and so any IO operations on the file
      will fail. [path] is only used with {!cloexec}, {!directory} and
      {!nofollow}, and any other flags will be ignored. *)

  val sync : t
  (** [sync] ensures that write operations on the file complete
      according to the requirements of synchronised IO file integrity
      completion. *)

  val tmpfile : t
  (** [tmpfile] creates an anonymous temporary regular file. The pathname
      must be a directory, within which an unnamed inode will be created.
      If [tmpfile] is specified without {!excl}, then a subsequent
      linkat call can move it permanently into the filesystem. *)

  val trunc : t
  (** [trunc] will set the file size to 0 if the file already exists
      and is a regular file and is opened for writing. If the file
      is a FIFO or terminal, then the flag is ignored. Use of [trunc]
      on other file types is unspecified. *)
end

(** Flags that can be passed to {!openat2} to control path resolution. *)
module Resolve : sig
  include FLAGS

  val beneath : t
  (** [beneath] does not permit path resolution to succeed if any
      component of the resolution is not a descendant of the directory
      indicated by the [dirfd] passed to the open call.  Absolute symbolic
      links and absolute pathnames will be rejected.

      For maximum compatiblity with future Linux kernels, the {!no_magiclinks}
      flag should be specified along with this one. *)

  val in_root : t
  (** [in_root] treats the [dirfd] directory as the root directory while
      resolving the pathname.  Absolute symbolic links are interpreted
      relative to the [dirfd].  If a prefix component of the pathname
      equates to the [dirfd], then an immediately following [..] component
      likewise equates to the [dirfd] (just as [/..] is traditionally
      equivalent to [/]). An absolute pathname is interpreted relative to
      the [dirfd].

      For maximum compatiblity with future Linux kernels, the {!no_magiclinks}
      flag should be specified along with this one. *)

  val no_magiclinks : t
  (** [no_magiclinks] disallows all magic-link resolution during path
      resolution.  Magic-links are symbolic link-like objects that are
      usually found in the [/proc] filesystem.  Unknowingly opening magic
      links can be risky for some applications, notably those without a
      controlling terminal or those within a containerised environment that
      may provide an escape vector. *)

  val no_symlinks : t
  (** [no_symlinks] disallows the resolution of symbolic links during path
      resolution, and implies the use of {!no_magiclinks}.  If the basename
      component of the pathname is a symlink, and [no_symlinks] is specified
      along with {!Open_flags.path} and {!Open_flags.nofollow}, then a fd
      referencing the symbolic link will be returned.

      Note that the [no_symlinks] flag affects the treatment of symbolic links
      in all of the components of pathname. This differs from the effect
      of the {!Open_flags.nofollow} file creation flag, which affects the
      handling of symbolic links only in the final component of the pathname. *)

  val no_xdev : t
  (** [no_xdev] disallows the traversal of mount points during path resolution,
      including bind mounts The pathname must either be on the same mount as
      the directory referred to by the [dirfd], or on the same mount as the
      current working directory if [dirfd] is not specified. *)

  val cached : t
  (** [cached] makes the open operation fail unless all path components are
      already present in the kernel lookup cache. Any revalidation or IO
      needed to satisfy the lookup will result in a {!Unix.EAGAIN} error. *)
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

module Linkat_flags : sig
  include FLAGS

  val empty_path : t
  (** If the old path is empty, link to old_dir_fd. *)

  val symlink_follow : t
  (** If the old path is a symlink, link its target. *)
end

val linkat : 'a t ->
  ?old_dir_fd:Unix.file_descr ->
  ?new_dir_fd:Unix.file_descr ->
  flags:Linkat_flags.t ->
  old_path:string ->
  new_path:string ->
  'a -> 'a job option
(** [linkat t ~flags ~old_path ~new_path] creates a new hard link.

    If [new_path] already exists then it is not overwritten.

    @param old_dir_fd If provided and [old_path] is a relative path, it is interpreted relative to [old_dir_fd].
    @param new_dir_fd If provided and [new_path] is a relative path, it is interpreted relative to [new_dir_fd].
    @param old_path Path of the already-existing link.
    @param new_path Path for the newly created link. *)

val unlink : 'a t -> dir:bool -> ?fd:Unix.file_descr -> string -> 'a -> 'a job option
(** [unlink t ~dir ~fd path] removes the directory entry [path], which is resolved relative to [fd].
    If [fd] is not given, then the current working directory is used.
    If [path] is a symlink, the link is removed, not the target.
    @param dir If [true], this acts like [rmdir] (only removing empty directories).
               If [false], it acts like [unlink] (only removing non-directories). *)

val mkdirat : 'a t -> mode:Unix.file_perm -> ?fd:Unix.file_descr -> string -> 'a -> 'a job option
(** [mkdirat t ~mode ~fd path] makes a directory [path], which is resolved relative to [fd].
    If [fd] is not given, then the current working directory is used.
    @param mode The mode used to create the directory. *)

module Poll_mask : sig
  include FLAGS

  val pollin  : t
  val pollout : t
  val pollerr : t
  val pollhup : t
end

val poll_add : 'a t -> Unix.file_descr -> Poll_mask.t -> 'a -> 'a job option
(** [poll_add t fd mask d] will submit a [poll(2)] request to uring [t].
    It completes and returns [d] when an event in [mask] is ready on [fd].
    This is an asynchronous version of poll(2). The operation will complete when
    any of the requested events occur on the file descriptor.

    The completion's [result] field contains:
    - On success: The bitwise OR of events that occurred (always a subset of [mask])
    - On error: A negative error code

    @param fd File descriptor to monitor
    @param mask Bitwise OR of events to monitor (see {!Poll_mask})
    @return [None] if the submission queue is full; otherwise [Some job] *)

type offset := Optint.Int63.t
(** For files, give the absolute offset, or use [Optint.Int63.minus_one] for the current position.
    For sockets, use an offset of [Optint.Int63.zero] ([minus_one] is not allowed here). *)

val read : 'a t -> file_offset:offset -> Unix.file_descr -> Cstruct.t -> 'a -> 'a job option
(** [read t ~file_offset fd buf d] will submit a [read(2)] request to uring [t].
    It reads from absolute [file_offset] on the [fd] file descriptor and writes
    the results into the memory pointed to by [buf].  The user data [d] will
    be returned by {!wait} or {!get_cqe_nonblocking} upon completion.

    The completion's [result] field contains the number of bytes read on success,
    0 for end-of-file, or a negative error code on failure.

    @param file_offset Use {!Optint.Int63.minus_one} for current file position,
                       or a specific offset for files. For sockets, use {!Optint.Int63.zero}
    @return [None] if the submission queue is full; otherwise [Some job] *)

val write : 'a t -> file_offset:offset -> Unix.file_descr -> Cstruct.t -> 'a -> 'a job option
(** [write t ~file_offset fd buf d] will submit a [write(2)] request to uring [t].
    It writes to absolute [file_offset] on the [fd] file descriptor from the
    the memory pointed to by [buf].  The user data [d] will be returned by
    {!wait} or {!get_cqe_nonblocking} upon completion.

    The completion's [result] field contains the number of bytes written on success,
    or a negative error code on failure. Note that a short write (less than the
    buffer size) is not an error.

    @param file_offset Use {!Optint.Int63.minus_one} for current file position,
                       or a specific offset for files. For sockets, use {!Optint.Int63.zero}
    @return [None] if the submission queue is full; otherwise [Some job] *)

val iov_max : int
(** The maximum length of the list that can be passed to {!readv} and {!writev}. *)

val readv : 'a t -> file_offset:offset -> Unix.file_descr -> Cstruct.t list -> 'a -> 'a job option
(** [readv t ~file_offset fd iov d] will submit a [readv(2)] request to uring [t].
    It reads from absolute [file_offset] on the [fd] file descriptor and writes
    the results into the memory pointed to by [iov].  The user data [d] will
    be returned by {!wait} or {!get_cqe_nonblocking} upon completion.

    This performs a vectored read, reading data into multiple buffers in a single
    operation. The completion's [result] field contains the total number of bytes
    read across all buffers, or a negative error code.

    @param file_offset File offset (see {!type:offset} for special values)
    @param iov List of buffers to read into
    @return [None] if the submission queue is full; otherwise [Some job]
    @raise Invalid_argument if [List.length iov > Uring.iov_max] *)

val writev : 'a t -> file_offset:offset -> Unix.file_descr -> Cstruct.t list -> 'a -> 'a job option
(** [writev t ~file_offset fd iov d] will submit a [writev(2)] request to uring [t].
    It writes to absolute [file_offset] on the [fd] file descriptor from the
    the memory pointed to by [iov].  The user data [d] will be returned by
    {!wait} or {!get_cqe_nonblocking} upon completion.

    This performs a vectored write, writing data from multiple buffers in a single
    operation. The completion's [result] field contains the total number of bytes
    written from all buffers, or a negative error code.

    @param file_offset File offset (see {!type:offset} for special values)
    @param iov List of buffers to write from
    @return [None] if the submission queue is full; otherwise [Some job]
    @raise Invalid_argument if [List.length iov > Uring.iov_max] *)

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

    This is a zero-copy data transfer between two file descriptors. At least one
    must be a pipe. Data is moved without copying between kernel and user space.

    The completion's [result] field contains the number of bytes transferred on success,
    0 for end-of-input, or a negative error code.

    @param src Source file descriptor (can be a regular file or pipe)
    @param dst Destination file descriptor (can be a regular file or pipe)
    @param len Maximum number of bytes to transfer
    @return [None] if the submission queue is full; otherwise [Some job] *)

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
  (** [pp_kind kind] formats a human readable [kind] *)

  val create : unit -> t
  (** Use [create] to make a statx result buffer to pass to {! statx}. *)

  module Flags : sig
    include FLAGS
    
    val empty_path : t
    (** [empty_path] signals that if the pathname is an empty string
        then operate on the file referred to by the [fd] (which can
        refer to any type of file).
        If [fd] is not specified then the call operates on the current
        working directory. *)

    val no_automount : t
    (** [no_automount] signals that statx should not automount the basename
        component of the path if it is an automount point.
        This can be used in tools that scan directories to prevent
        mass-automounting of a directory of automount points. *)

    val symlink_nofollow : t
    (** [symlink_nofollow] signals that if the path is a symbolic link,
        then return information about the link itself. *)

    val statx_sync_as_stat : t
    (** [statx_sync_as_stat] is the filesystem-specific behaviour in
        response to stat calls. *)

    val statx_force_sync : t
    (** [statx_force_sync] forces synchronisation with the server, if
        the filesystem is a network-backed one. *)

    val statx_dont_sync : t
    (** [statx_dont_sync] signals that locally cached timestamps are
        sufficient, if run on a network-backed filesystem. *)
  end

  module Attr : sig
    include FLAGS

    val compressed : t
    (** The file is compressed by the filesystem. *)

    val immutable : t
    (** The file cannot be modified, as defined by chattr(1). *)

    val append : t
    (** The file can only be opened in append mode for writing.
        See chattr(1). *)

    val nodump : t
    (** The file is not a candidate for backup when a backup
        program scans the filesystem. *)

    val encrypted : t
    (** A key is required for the file to be encrypted by the
        filesystem. *)

    val verity : t
    (** The file has fs-verity enabled.  It cannot be written to,
        and all reads from it will be verified against a
        cryptographic hash that covers the entire file. *)

    val dax : t
    (** The file is in the DAX (cpu direct access) state, which
        minimises page-cache effects for both I/O and memory mappings
        of this file.
        @see <https://www.kernel.org/doc/Documentation/filesystems/dax.txt> Direct Access for Files
        @since Linux 5.8 *)

    val check : ?mask:Int64.t -> Int64.t -> t -> bool
    (** [check ?mask attr t] will check if [t] is set in [attr].

        If [mask] is not [None] then it will first check the mask to see
        if the file attribute is supported and if not raise [Invalid_argument]. *)
  end

  module Mask : sig
    (** The mask flags are used to tell the kernel which fields the {!statx} invocation
        is interested in. You may wish to use {! Mask.check} on the returned {!Statx.t} to
        verify the field has actually been filled in with a sensible value first.
        In general, the kernel never refused values specified in the mask, but may choose
        to not set the mask in the returned buffer from {!statx}. *)

    include FLAGS

    val type' : t
    (** Retrieve the kind of file field, accessible afterwards via {!val:Statx.kind}. *)

    val mode : t
    (** Retrieve the permissions field, accessible afterwards via {!Statx.perm}. *)

    val nlink : t
    (** Retrieve the number of links field, accessible afterwards via {!Statx.nlink}. *)

    val uid : t
    (** Retrieve the user ID field, accessible afterwards via {!Statx.uid}. *)

    val gid : t
    (** Retrieve the group ID field, accessible afterwards via {!Statx.gid}. *)

    val atime : t
    (** Retrieve the last access field, accessible afterwards
        via {!atime_nsec} and {!atime_sec}. *)

    val mtime : t
    (** Retrieve the last modification field, accessible afterwards
        via {!mtime_nsec} and {!mtime_sec}. *)

    val ctime : t
    (** Retrieve the last status change field, accessible afterwards
        via {!ctime_nsec} and {!ctime_sec}. *)

    val ino : t
    (** Retrieve the inode number, accessible afterwards via {!Statx.ino}. *)

    val size : t
    (** Retrieve the total size in bytes, accessible afterwards via {!Statx.size}. *)

    val blocks : t
    (** Retrieve the number of 512B blocks allocate, accessible afterwards via {!Statx.blocks}. *)

    val basic_stats : t
    (** Retrieve all of the above flags. *)

    val btime : t
    (** Retrieve the birthtime field, accessible afterwards via {!btime_nsec} and {!btime_sec}. *)

    val mnt_id : t
    (** @since Linux 5.8 *)

    val dioalign : t
    (** @since Linux 6.1 *)

    val check : Int64.t -> t -> bool
    (** [check mask t] checks if [t] is set in [mask]. *)
  end

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

val bind : 'a t -> Unix.file_descr -> Unix.sockaddr -> 'a -> 'a job option
(** [bind t fd addr d] will submit a request to bind socket [fd] to network address [addr].

    This is an asynchronous version of bind(2). The socket should typically be created
    with [Unix.SOCK_NONBLOCK] to work well with io_uring. The completion will have
    [result = 0] on success, or a negative error code on failure.

    @return [None] if the submission queue is full; otherwise [Some job] *)

val listen : 'a t -> Unix.file_descr -> int -> 'a -> 'a job option
(** [listen t fd backlog d] will submit a request to mark socket [fd] as passive,
    ready to accept incoming connections.

    This is an asynchronous version of listen(2). The [backlog] parameter defines
    the maximum length of the queue of pending connections. If a connection request
    arrives when the queue is full, the client may receive an ECONNREFUSED error.
    The completion will have [result = 0] on success.

    @param fd Socket file descriptor (must be already bound with {!bind})
    @param backlog Maximum number of pending connections (often capped by system limits)
    @return [None] if the submission queue is full; otherwise [Some job]
    @raise Invalid_argument if [fd] is not a socket *)

val connect : 'a t -> Unix.file_descr -> Unix.sockaddr -> 'a -> 'a job option
(** [connect t fd addr d] will submit a request to connect socket [fd] to [addr].

    This is an asynchronous version of connect(2). For non-blocking sockets,
    the operation may initially return an error indicating the connection is
    in progress, then completes with [result = 0] when established or a
    negative error code on failure.

    @return [None] if the submission queue is full; otherwise [Some job] *)

(** Holder for the peer's address in {!accept}. *)
module Sockaddr : sig
  type t

  val create : unit -> t
  val get : t -> Unix.sockaddr
end

val accept : 'a t -> Unix.file_descr -> Sockaddr.t -> 'a -> 'a job option
(** [accept t fd addr d] will submit a request to accept a new connection on [fd].
    The new FD will be configured with [SOCK_CLOEXEC].
    The remote address will be stored in [addr].

    This is an asynchronous version of accept4(2) with SOCK_CLOEXEC flag.
    The completion's [result] field contains the new file descriptor on success,
    or a negative error code on failure.

    @param fd Listening socket (must have called {!listen} first)
    @param addr Pre-allocated storage for the peer address (create with {!Sockaddr.create})
    @return [None] if the submission queue is full; otherwise [Some job] *)

val close : 'a t -> Unix.file_descr -> 'a -> 'a job option
(** [close t fd d] will submit a request to close file descriptor [fd].

    This is an asynchronous version of close(2). The completion's [result]
    field will be 0 on success or a negative error code.

    Note: Even on error, the file descriptor is considered closed and should
    not be used again. The descriptor will not be reused until the operation
    completes.

    @return [None] if the submission queue is full; otherwise [Some job] *)

val cancel : 'a t -> 'a job -> 'a -> 'a job option
(** [cancel t job d] submits a request to cancel [job].

    Cancellation is asynchronous - the original operation may still complete
    before the cancellation takes effect. Both the original operation and the
    cancel operation will generate completion events.

    @param job The job handle returned when the operation was submitted
    @return [None] if the submission queue is full; otherwise [Some cancel_job]
    @raise Invalid_argument if the job has already been collected by {!wait} or {!get_cqe_nonblocking} *)

module Msghdr : sig
  type t

  val create : ?n_fds:int -> ?addr:Sockaddr.t -> Cstruct.t list -> t
  (** [create buffs] makes a new [msghdr] using the [buffs]
      for the underlying [iovec].

      Requires [List.length buffs <= Uring.iov_max]

      @param addr The remote address.
                  Use {!Sockaddr.create} to create a dummy address that will be filled when data is received.
      @param n_fds Reserve space to receive this many FDs (default 0) *)

  val get_fds : t -> Unix.file_descr list
end

val send_msg : ?fds:Unix.file_descr list -> ?dst:Unix.sockaddr -> 'a t -> Unix.file_descr -> Cstruct.t list -> 'a -> 'a job option
(** [send_msg t fd buffs d] will submit a [sendmsg(2)] request. The [Msghdr] will be constructed
    from the FDs ([fds]), address ([dst]) and buffers ([buffs]).

    This is useful for:
    - Sending to unconnected sockets (UDP) with [dst]
    - Sending file descriptors over Unix domain sockets with [fds]
    - Scatter-gather I/O with multiple buffers

    The completion's [result] field contains the number of bytes sent on success,
    or a negative error code.

    @param dst Destination address for unconnected sockets
    @param fds File descriptors to send via SCM_RIGHTS (Unix domain sockets only)
    @return [None] if the submission queue is full; otherwise [Some job]
    @raise Invalid_argument if [List.length buffs > Uring.iov_max] *)

val recv_msg : 'a t -> Unix.file_descr -> Msghdr.t -> 'a -> 'a job option
(** [recv_msg t fd msghdr d] will submit a [recvmsg(2)] request. If the request is
    successful then the [msghdr] will contain the sender address and the data received.

    This is useful for:
    - Receiving from unconnected sockets (UDP) - sender address is stored
    - Receiving file descriptors over Unix domain sockets
    - Scatter-gather I/O with multiple buffers

    The completion's [result] field contains the number of bytes received on success,
    or a negative error code. Use {!Msghdr.get_fds} to retrieve any received file
    descriptors.

    @param msghdr Pre-allocated message header created with {!Msghdr.create}
    @return [None] if the submission queue is full; otherwise [Some job] *)

val fsync : 'a t -> ?off:int64 -> ?len:int -> Unix.file_descr -> 'a -> 'a job option
(** [fsync t ?off ?len fd d] will submit an [fsync(2)] request, with the optional
    offset [off] and length [len] specifying the subset of the file to perform the
    synchronisation on.

    This ensures that all file data and metadata are durably stored on disk.
    The completion's [result] field will be 0 on success or a negative error code.

    @param off Starting offset for sync range (requires kernel 5.2+)
    @param len Length of range to sync; if both [off] and [len] are given,
               only that range is synced (requires kernel 5.2+)
    @return [None] if the submission queue is full; otherwise [Some job] *)

val fdatasync : 'a t -> ?off:int64 -> ?len:int -> Unix.file_descr -> 'a -> 'a job option
(** [fdatasync t ?off ?len fd d] will submit an [fdatasync(2)] request, with the optional
    offset [off] and length [len] specifying the subset of the file to perform the
    synchronisation on.

    Like {!fsync} but only ensures file data (not metadata) is durably stored.
    This can be more efficient when file metadata (permissions, timestamps) hasn't changed.
    The completion's [result] field will be 0 on success or a negative error code.

    @param off Starting offset for sync range (requires kernel 5.2+)
    @param len Length of range to sync
    @return [None] if the submission queue is full; otherwise [Some job] *)

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

    When a completion event is posted to the CQ ring, the eventfd will be signaled.
    This allows integration with event loops like epoll/select. The eventfd should
    be created with [Unix.eventfd] or similar.

    Only one eventfd can be registered per ring. Registering a new one replaces
    the previous registration.

    @param fd An eventfd file descriptor
    @raise Unix.Unix_error on registration failure *)

val error_of_errno : int -> Unix.error
(** [error_of_errno e] converts the error code [abs e] to a Unix error type. *)

val active_ops : _ t -> int
(** [active_ops t] returns the number of operations added to the ring (whether submitted or not)
    for which the completion event has not yet been collected.

    This is useful for:
    - Ensuring all operations complete before calling {!exit}
    - Monitoring ring utilization
    - Detecting potential ring overflow conditions

    The count includes operations that are queued but not submitted, submitted
    but not completed, and completed but not collected via {!wait} or {!get_cqe_nonblocking}. *)

val sqe_ready : _ t -> int
(** [sqe_ready t] is the number of unconsumed (if SQPOLL) or unsubmitted entries in the SQ ring. *)

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
