let ( = ) : int -> int -> bool = ( = )
let ( <> ) : int -> int -> bool = ( <> )

type ptr = int
let slot_taken = -1
let free_list_nil = -2

(* Free-list allocator *)
type 'a t =
  { data: 'a array
  (* Pool of potentially-empty data slots. Invariant: an unfreed pointer [p]
     into this array is valid iff [free_tail_relation.(p) = slot_taken]. *)
  ; mutable free_head: ptr
  ; free_tail_relation: ptr array
  (* A linked list of pointers to free slots, with [free_head] being the first
     element and [free_tail_relation] mapping each free slot to the next one.
     Each entry [x] signals a state of the corresponding [data.(x)] slot:

     - [x = slot_taken]: the data slot is taken;
     - [x = free_list_nil]: the data slot is free, and is last to be allocated;
     - [0 <= x < length]: the data slot is free, and will be allocated before
       [free_tail_relation.(x)].

     The user is given only pointers [p] such that [free_tail_relation.(p) =
     slot_taken]. *)
  ; length: int
  }

let create : type a. int -> a t =
 fun n ->
  if n < 0 || n > Sys.max_array_length then invalid_arg "Heap.create" ;
  (* Every slot is free, and all but the last have a free successor. *)
  let free_head = if n = 0 then free_list_nil else 0 in
  let free_tail_relation = Array.init n succ in
  if n > 0 then free_tail_relation.(n - 1) <- free_list_nil;
  let data =
    (* No slot in [free_tail_relation] is [slot_taken], so initial data is
       inaccessible. *)
    Array.make n (Obj.magic `invalid : a)
  in
  { data; free_head; free_tail_relation; length = n }

exception No_space

let alloc t a =
  let ptr = t.free_head in
  if ptr = free_list_nil then raise No_space;
  Array.unsafe_set t.data ptr a;

  (* Drop [ptr] from the free list. *)
  let tail = Array.unsafe_get t.free_tail_relation ptr in
  Array.unsafe_set t.free_tail_relation ptr slot_taken;
  t.free_head <- tail;

  ptr

let free : type a. a t -> ptr -> a =
 fun t ptr ->
  assert (ptr >= 0) (* [alloc] returns only valid pointers. *);
  if ptr >= t.length then Fmt.invalid_arg "Heap.free: invalid pointer %d" ptr;
  let slot_state = Array.unsafe_get t.free_tail_relation ptr in
  if slot_state <> slot_taken then invalid_arg "Heap.free: pointer already freed";

  (* [t.free_tail_relation.(ptr) = slot_taken], so [t.data.(ptr)] is valid. *)
  let datum = Array.unsafe_get t.data ptr in

  (* Cons [ptr] to the free-list. *)
  Array.unsafe_set t.free_tail_relation ptr t.free_head;
  t.free_head <- ptr;

  (* We've marked this slot as free, so [t.data.(ptr)] is inaccessible. We zero
     it to allow it to be GC-ed. *)
  assert (t.free_tail_relation.(ptr) <> slot_taken);
  Array.unsafe_set t.data ptr (Obj.magic `invalid : a);

  datum
