let ( = ) : int -> int -> bool = ( = )
let ( <> ) : int -> int -> bool = ( <> )

type ptr = int
let null_ptr = -1
let is_null x = x = null_ptr

(* Free-list allocator *)
type 'a t =
  { data: 'a array
  (* Pool of potentially-empty data slots. Invariant: an unfreed pointer [p]
     into this array is valid if one of the following holds:

     1. [is_null free_head];
     2. [not (is_null free_head)
         && free_head <> p
         && is_null free_tail_relation.(p)].

     Either all slots are occupied (1), or there's at least one free slot and
     [p] is not one of them (2). *)
  ; mutable free_head: ptr
  ; free_tail_relation: ptr array
  (* A linked list of free blocks, with [free_head] being the first element and
     [free_tail_relation] mapping each free slot [p] to the next one (or
     [null_ptr] if [p] is either not free or is the final slot).

     Invariant: for all [p : ptr], either [is_null p] or [0 <= p < length]. The
     user sees non-null pointers only. *)
  ; max_size: int
  }

let create : type a. int -> a t =
 fun n ->
  if n < 0 || n > Sys.max_array_length then invalid_arg "Heap.create" ;
  (* Every slot is free, and all but the last have a free successor. *)
  let free_head = if n = 0 then null_ptr else 0 in
  let free_tail_relation = Array.init n succ in
  if n > 0 then free_tail_relation.(n - 1) <- null_ptr;
  let data =
    (* See invariants on {!t}, which ensure this data is never read. *)
    Array.make n (Obj.magic `invalid : a)
  in
  { data; free_head; free_tail_relation; max_size = n }

exception No_space

let alloc t a =
  let ptr = t.free_head in
  if is_null ptr then raise No_space;
  let next_head = t.free_tail_relation.(ptr) in
  Array.unsafe_set t.free_tail_relation ptr null_ptr;
  Array.unsafe_set t.data ptr a;
  t.free_head <- next_head;
  ptr

let free : type a. a t -> ptr -> a =
 fun t ptr ->
  let () =
    assert (not (is_null ptr)) (* [alloc] returns only valid pointers. *);
    if ptr >= t.max_size then invalid_arg "Heap.free: invalid pointer";
    let next_free = Array.unsafe_get t.free_tail_relation ptr in

    if is_null t.free_head then
      (* No free slots, so [ptr] is valid in [t.data]. Case (1) above. *)
      assert (is_null next_free)

    else if t.free_head <> ptr && is_null next_free then
      (* One or more free slots, and [ptr] isn't one of them. Case (2) above. *)
      Array.unsafe_set t.free_tail_relation ptr t.free_head

    else invalid_arg "Heap.free: pointer already freed"
  in
  t.free_head <- ptr;
  let datum = Array.unsafe_get t.data ptr in
  (* Zero the freed slot. Since this value is now inaccessible (with [ptr] is
      in the free list) we might as well let it be GCed. *)
  Array.unsafe_set t.data ptr (Obj.magic `invalid : a);
  datum
