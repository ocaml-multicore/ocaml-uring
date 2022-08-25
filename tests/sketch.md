```ocaml
# #require "uring";;
```

## Sketch allocation

```ocaml
module Int63 = Optint.Int63

let ldup n x = List.init n (Fun.const x)

let rec consume t =
  match Uring.wait t with
  | Some { data; result } -> (data, result)
  | None -> consume t
```

```ocaml
# let t : unit Uring.t = Uring.create ~queue_depth:4 ();;
val t : unit Uring.t = <abstr>
# let fd = Unix.openfile "/dev/zero" [ O_RDONLY ] 0;;
val fd : Unix.file_descr = <abstr>
# let b = Cstruct.create 1;;
val b : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 1}

# Uring.readv t fd (ldup 1 b) () ~file_offset:Int63.zero;;
- : unit Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1

# consume t;;
- : unit * int = ((), 1)

# Uring.readv t fd (ldup 7 b) () ~file_offset:Int63.zero;;
- : unit Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# consume t;;
- : unit * int = ((), 7)

# Uring.readv t fd (ldup 1000 b) () ~file_offset:Int63.zero;;
- : unit Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# consume t;;
- : unit * int = ((), 1000)

# let fd : unit = Unix.close fd;;
val fd : unit = ()
# Uring.exit t;;
- : unit = ()
```
