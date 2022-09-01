```ocaml
# #require "uring";;
```

# Heap tests

```ocaml
module Heap = struct
  include Uring.Private.Heap
  let alloc = alloc ~extra_data:()
  let alloc_no_growth = alloc_no_growth ~extra_data:()
end

let random_hashtbl_elt tbl =
  let rec inner n acc (seq : _ Seq.t) =
    match seq () with
    | Nil -> acc
    | Cons (x, xf) ->
      let acc = if Random.int n = 0 then x else acc in
      inner (succ n) acc xf
  in
  match Hashtbl.to_seq tbl () with
  | Nil -> invalid_arg "random_hashtbl_elt"
  | Cons (x, xf) -> inner 1 x xf
```

Test normal usage:

```ocaml
# let max_size = 10 in
  let t = Heap.create max_size in
  let reference : (Heap.ptr, int) Hashtbl.t = Hashtbl.create max_size in
  let currently_allocated = ref 0 in
  for _ = 1 to 100_000 do
    let attempt_alloc = !currently_allocated = 0 || Random.bool () in
    match attempt_alloc with
    | true ->
      if !currently_allocated = max_size then
        try ignore (Heap.alloc_no_growth t 0); assert false
        with Heap.No_space -> ()
      else
        let data = Random.int 5000 in
        let ptr = Heap.ptr (Heap.alloc_no_growth t data) in
        assert (not (Hashtbl.mem reference ptr));
        Hashtbl.add reference ptr data;
        incr currently_allocated
    | false ->
      let (k, v) = random_hashtbl_elt reference in
      let v' = Heap.free t k in
      Hashtbl.remove reference k;
      assert (v = v');
      decr currently_allocated
  done;
  Hashtbl.iter (fun k _ -> ignore (Heap.free t k)) reference;
  Heap.release t;;
- : unit = ()
```

Double free in an empty heap:

```ocaml
# let t : int Heap.t = Heap.create 1;;
val t : int Heap.t = <abstr>
# let p = Heap.ptr @@ Heap.alloc t 1;;
val p : Heap.ptr = 0
# Heap.free t p;;
- : int = 1
# Heap.free t p;;
Exception: Invalid_argument "Heap.free: pointer already freed".
# let t : unit = Heap.release t;;
val t : unit = ()
```

Double free in a non-empty heap:

```ocaml
# let t : int Heap.t = Heap.create 2;;;
val t : int Heap.t = <abstr>
# let p1 = Heap.ptr @@ Heap.alloc t 1;;;
val p1 : Heap.ptr = 0
# let p2 = Heap.ptr @@ Heap.alloc t 2;;;
val p2 : Heap.ptr = 1
# Heap.free t p1;;
- : int = 1
# Heap.free t p1;;
Exception: Invalid_argument "Heap.free: pointer already freed".
# Heap.free t p2;;
- : int = 2
# let t : unit = Heap.release t;;
val t : unit = ()
```

Out of space:

```ocaml
# let t : unit Heap.t = Heap.create 0 (* 1 > 0 *);;
val t : unit Heap.t = <abstr>
# Heap.ptr @@ Heap.alloc_no_growth t ();;
Exception: Uring__Heap.No_space.
# let t : unit = Heap.release t;;
val t : unit = ()
```

```ocaml
# let t : unit Heap.t = Heap.create 2;;
val t : unit Heap.t = <abstr>
# let p1 = Heap.ptr @@ Heap.alloc_no_growth t ();;
val p1 : Heap.ptr = 0
# let p2 = Heap.ptr @@ Heap.alloc_no_growth t ();;
val p2 : Heap.ptr = 1
# Heap.ptr @@ Heap.alloc_no_growth t () (* 3 > 2 *);;
Exception: Uring__Heap.No_space.
# List.iter (fun p -> ignore (Heap.free t p)) [p1; p2];;
- : unit = ()
# let t : unit = Heap.release t;;
val t : unit = ()
```

```ocaml
# let t : int Heap.t = Heap.create 3;;
val t : int Heap.t = <abstr>
# let p1 = Heap.ptr @@ Heap.alloc_no_growth t 1;;
val p1 : Heap.ptr = 0
# let p2 = Heap.ptr @@ Heap.alloc_no_growth t 2;;
val p2 : Heap.ptr = 1
# Heap.free t p1;;
- : int = 1
# let p3 = Heap.ptr @@ Heap.alloc_no_growth t 3;;
val p3 : Heap.ptr = 0
# let p4 = Heap.ptr @@ Heap.alloc_no_growth t 4;;
val p4 : Heap.ptr = 2
# Heap.ptr @@ Heap.alloc_no_growth t 5  (* 2 - 1 + 3 > 3 *);;
Exception: Uring__Heap.No_space.
# List.iter (fun p -> ignore (Heap.free t p)) [p2; p3; p4];;
- : unit = ()
# let t : unit = Heap.release t;;
val t : unit = ()
```

Growth:
```ocaml
let shuffle_list l =
  List.map (fun i -> Random.bits (), i) l |>
  List.sort (fun a b -> compare (fst a) (fst b)) |>
  List.map snd
```

```ocaml
# let t = Heap.create 0 in
  let add_l = List.init 1024 (fun i -> i) |> shuffle_list in
  assert (Heap.in_use t = 0);
  let free_l = List.map (fun i -> Heap.alloc t i |> Heap.ptr) add_l |>
    shuffle_list
  in
  assert (Heap.in_use t = 1024);
  List.iter (fun p -> ignore (Heap.free t p)) free_l;
  assert (Heap.in_use t = 0);
  Heap.release t;;
- : unit = ()
```
