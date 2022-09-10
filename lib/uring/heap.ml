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

let ( = ) : int -> int -> bool = ( = )
let ( <> ) : int -> int -> bool = ( <> )

type ptr = int
let slot_taken = -1
let free_list_nil = -2

(* [extra_data] is for keeping pointers passed to C alive. *)
type 'a entry =
  | Empty : 'a entry
  | Entry : { data : 'a; extra_data : 'b; mutable ptr : int } -> 'a entry

(* Free-list allocator *)
type 'a t =
  { mutable data: 'a entry array
  (* Pool of potentially-empty data slots. Invariant: an unfreed pointer [p]
     into this array is valid iff [free_tail_relation.(p) = slot_taken]. *)
  ; mutable free_head: ptr
  ; mutable free_tail_relation: ptr array
  (* A linked list of pointers to free slots, with [free_head] being the first
     element and [free_tail_relation] mapping each free slot to the next one.
     Each entry [x] signals a state of the corresponding [data.(x)] slot:

     - [x = slot_taken]: the data slot is taken;
     - [x = free_list_nil]: the data slot is free, and is last to be allocated;
     - [0 <= x < length data]: the data slot is free, and will be allocated before
       [free_tail_relation.(x)].

     The user is given only pointers [p] such that [free_tail_relation.(p) =
     slot_taken]. *)
  ; mutable in_use: int         (* Negative after release *)
  }

let ptr = function
  | Entry { ptr = -1; _ } -> invalid_arg "Entry has already been freed!"
  | Entry { ptr; _ } -> ptr
  | Empty -> assert false

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
    Array.make n Empty
  in
  { data; free_head; free_tail_relation; in_use = 0 }

exception No_space

let in_use t = t.in_use

let release t =
  if t.in_use > 0 then invalid_arg "Heap still in use!"
  else if t.in_use < 0 then invalid_arg "Heap already released!";
  t.in_use <- -100;
  t.free_head <- free_list_nil

let is_released t = t.in_use < 0

(* Note: t must be full *)
let grow t =
  if is_released t then raise No_space;
  if t.free_head <> free_list_nil then invalid_arg "Heap is not full";
  let old_len = Array.length t.free_tail_relation in
  if old_len = Sys.max_array_length then
    raise No_space;
  let new_len = min (max 64 (old_len * 2)) Sys.max_array_length in
  (* Build new t.free_tail_relation, keep in sync with create() *)
  let new_free_tail_relation =
    Array.init new_len
      (fun i ->
         if i < old_len then
           t.free_tail_relation.(i)
         else succ i)
  in
  new_free_tail_relation.(new_len - 1) <- free_list_nil;
  (* First element of enlarged array *)
  let new_free_head = old_len in
  (* Note: Keep in sync with create() *)
  let new_data =
    Array.init new_len
      (fun i ->
         if i < old_len then
           t.data.(i)
         else
           Empty)
  in
  (* Commit *)
  t.free_tail_relation <- new_free_tail_relation;
  t.free_head <- new_free_head;
  t.data <- new_data

let alloc_no_growth t data ~extra_data =
  let ptr = t.free_head in
  if ptr = free_list_nil then raise No_space;
  let entry = Entry { data; extra_data; ptr } in
  t.data.(ptr) <- entry;

  (* Drop [ptr] from the free list. *)
  let tail = t.free_tail_relation.(ptr) in
  t.free_tail_relation.(ptr) <- slot_taken;
  t.free_head <- tail;
  t.in_use <- t.in_use + 1;

  entry

let alloc t data ~extra_data =
  try
    alloc_no_growth t data ~extra_data
  with
    No_space -> grow t; alloc_no_growth t data ~extra_data

let free t ptr =
  assert (ptr >= 0) (* [alloc] returns only valid pointers. *);
  if ptr >= Array.length t.data then Fmt.invalid_arg "Heap.free: invalid pointer %d" ptr;
  let slot_state = t.free_tail_relation.(ptr) in
  if slot_state <> slot_taken then invalid_arg "Heap.free: pointer already freed";

  (* [t.free_tail_relation.(ptr) = slot_taken], so [t.data.(ptr)] is valid. *)
  let datum =
    match t.data.(ptr) with
    | Empty -> assert false
    | Entry p ->
      p.ptr <- -1;
      p.data
  in

  (* Cons [ptr] to the free-list. *)
  t.free_tail_relation.(ptr) <- t.free_head;
  t.free_head <- ptr;

  (* We've marked this slot as free, so [t.data.(ptr)] is inaccessible. We zero
     it to allow it to be GC-ed. *)
  assert (t.free_tail_relation.(ptr) <> slot_taken);
  t.data.(ptr) <- Empty;         (* Extra-data can be GC'd here *)
  t.in_use <- t.in_use - 1;

  datum

