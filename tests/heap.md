```ocaml
# #require "uring";;
```

# Heap tests

```ocaml
module Heap = struct
  include Uring.Private.Heap
  let alloc = alloc ~extra_data:()
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
        try ignore (Heap.alloc t 0); assert false
        with Heap.No_space -> ()
      else
        let data = Random.int 5000 in
        let ptr = Heap.ptr (Heap.alloc t data) in
        assert (not (Hashtbl.mem reference ptr));
        Hashtbl.add reference ptr data;
        incr currently_allocated
    | false ->
      let (k, v) = random_hashtbl_elt reference in
      let v' = Heap.free t k in
      Hashtbl.remove reference k;
      assert (v = v');
      decr currently_allocated
  done;;
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
```

Double free in a non-empty heap:

```ocaml
# let t : int Heap.t = Heap.create 2;;;
val t : int Heap.t = <abstr>
# let p = Heap.ptr @@ Heap.alloc t 1;;;
val p : Heap.ptr = 0
# let _ = Heap.ptr @@ Heap.alloc t 2;;;
- : Heap.ptr = 1
# Heap.free t p;;
- : int = 1
# Heap.free t p;;
Exception: Invalid_argument "Heap.free: pointer already freed".
```

Out of space:

```ocaml
# let t : unit Heap.t = Heap.create 0 (* 1 > 0 *);;
val t : unit Heap.t = <abstr>
# Heap.ptr @@ Heap.alloc t ();;
Exception: Uring__Heap.No_space.
```

```ocaml
# let t : unit Heap.t = Heap.create 2;;
val t : unit Heap.t = <abstr>
# Heap.ptr @@ Heap.alloc t ();;
- : Heap.ptr = 0
# Heap.ptr @@ Heap.alloc t ();;
- : Heap.ptr = 1
# Heap.ptr @@ Heap.alloc t () (* 3 > 2 *);;
Exception: Uring__Heap.No_space.
```

```ocaml
# let t : int Heap.t = Heap.create 3;;
val t : int Heap.t = <abstr>
# let p1 = Heap.ptr @@ Heap.alloc t 1;;
val p1 : Heap.ptr = 0
# Heap.ptr @@ Heap.alloc t 2;;
- : Heap.ptr = 1
# Heap.free t p1;;
- : int = 1
# Heap.ptr @@ Heap.alloc t 3;;
- : Heap.ptr = 0
# Heap.ptr @@ Heap.alloc t 4;;
- : Heap.ptr = 2
# Heap.ptr @@ Heap.alloc t 5  (* 2 - 1 + 3 > 3 *);;
Exception: Uring__Heap.No_space.
```
