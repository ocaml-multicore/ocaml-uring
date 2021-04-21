type 'a t
(** A bounded heap of values of type ['a]. *)

val create : int -> _ t
(** [create n] is a heap that holds at most [n] elements. *)

type ptr = private int
(** A pointer to an element in a heap. *)

exception No_space

val alloc : 'a t -> 'a -> ptr
(** [alloc t a] adds the value [a] to [t] and returns a pointer to that value,
    or raises {!No_space} if no space exists in [t]. *)

val free : 'a t -> ptr -> 'a
(** [free t p] returns the element referenced by [p] and removes it from the
    heap. Has undefined behaviour if [p] has already been freed. *)
