(** [Region] handles carving up a block of external memory into
    smaller chunks.  This is currently just a slab allocator of
    a fixed size, on the basis that most IO operations operate on
    predictable chunks of memory. Since the block of memory in a
    region is contiguous, it can be used in Uring's fixed buffer
    model to map it into kernel space for more efficient IO. *)

type t
  (** [t] is a contiguous region of memory *)

  type chunk
  (** [chunk] is an offset into a region of memory allocated
      from some region [t].  It is of a length set when the
      region [t] associated with it was initialised. *)

  exception No_space
  (** [No_space] is raised when an allocation request cannot
      be satisfied. *)

  val init: block_size:int -> Iovec.Buffer.t -> int -> t
  (** [init ~block_size buf slots] initialises a region from
      the buffer [buf] with total size of [block_size * slots]. *)

  val alloc : t -> chunk
  (** [alloc t] will allocate a single chuck of length [block_size]
      from the region [t]. *)

  val free : chunk -> unit
  (** [free chunk] will return the memory [chunk] back to the region
      [t] where it can be reallocated. *)

  val length : chunk -> int
  (** [length chunk] is the block size. *)

  val to_offset : chunk -> int
  (** [to_offset chunk] will convert the [chunk] into an integer
      offset in its associated region.  This can be used in IO calls
      involving that memory. *)

  val to_bigstring : ?len:int -> chunk -> Iovec.Buffer.t
  (** [to_bigstring ?len chunk] will create a {!Bigarray} into the
      chunk of memory. Note that this is a zero-copy view into the
      underlying region [t] and so the [chunk] should not be freed
      until this Bigarray reference is no longer used.

      If [len] is specified then the returned view is of that size,
      and otherwise it defaults to [block_size]. *)

  val to_string : ?len:int -> chunk -> string
  (** [to_string ?len chunk] will return a copy of the [chunk]
      as an OCaml string.

      If [len] is specified then the returned view is of that size,
      and otherwise it defaults to [block_size]. *)

  val avail : t -> int
  (** [avail t] is the number of free chunks of memory remaining
      in the region. *)
