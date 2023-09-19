```ocaml
# #require "uring";;
```

# Uring tests

```ocaml
module Int63 = Optint.Int63

module Test_data = struct
  let path = "output_file.txt"

  let setup () =
    let oc = open_out path in
    output_string oc "A test file";
    close_out oc
end

let rec consume t =
  match Uring.wait ~timeout:1. t with
  | Some { data; result } -> (data, result)
  | None -> consume t

let traceln fmt =
  Format.printf (fmt ^^ "@.")

let () = Test_data.setup ()
```

Setup a new printer for bytes to make things readable.

```ocaml
# let pp_bytes ppf b = Format.fprintf ppf "Bytes.t <len:%i>" (Bytes.length b);;
val pp_bytes : Format.formatter -> bytes -> unit = <fun>
# #install_printer pp_bytes;;
```

## Queue depth

```ocaml
# Uring.create ~queue_depth:0 ();;
Exception: Invalid_argument "Non-positive queue depth: 0".
```

Prove we can wait more entries than queue depth

```ocaml
# let t : [ `Read ] Uring.t = Uring.create ~queue_depth:1 ();;
val t : [ `Read ] Uring.t = <abstr>

# let slab = Uring.Slab.create Uring.major_alloc_byte_size;;
val slab : Uring.Slab.t = <abstr>

# let fd = Unix.openfile "/dev/zero" Unix.[O_RDONLY] 0;;
val fd : Unix.file_descr = <abstr>
# let b = Uring.Slab.slice slab 1;;
val b : Uring.Bstruct.t = <abstr>
# Uring.read t fd b `Read ~file_offset:Int63.minus_one;;
- : [ `Read ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# Uring.read t fd b `Read ~file_offset:Int63.minus_one;;
- : [ `Read ] Uring.job option = Some <abstr>
# Uring.read t fd b `Read ~file_offset:Int63.minus_one;;
- : [ `Read ] Uring.job option = None
# Uring.submit t;;
- : int = 1
# consume t;;
- : [ `Read ] * int = (`Read, 1)
# consume t;;
- : [ `Read ] * int = (`Read, 1)
# let fd : unit = Unix.close fd;;
val fd : unit = ()
# Uring.exit t;;
- : unit = ()
```

## Noop

```ocaml
# let queue_depth = 5;;
val queue_depth : int = 5

# let t : int Uring.t = Uring.create ~queue_depth ();;
val t : int Uring.t = <abstr>

# Fmt.pr "%a@." Uring.Stats.pp (Uring.get_debug_stats t);;
SQEs ready: 0
Operations active: 0
Sketch buffer: 0/0 (plus 0 old buffers)
- : unit = ()

# for i = 1 to queue_depth do
    assert (Option.is_some (Uring.noop t i));
  done;;
- : unit = ()

# Fmt.pr "%a@." Uring.Stats.pp (Uring.get_debug_stats t);;
SQEs ready: 5
Operations active: 5
Sketch buffer: 0/0 (plus 0 old buffers)
- : unit = ()

# Uring.submit t;;
- : int = 5

# Fmt.pr "%a@." Uring.Stats.pp (Uring.get_debug_stats t);;
SQEs ready: 0
Operations active: 5
Sketch buffer: 0/0 (plus 0 old buffers)
- : unit = ()

# for i = 1 to queue_depth do
    let tkn, res = consume t in
    traceln "%d returned %d" tkn res;
  done;;
1 returned 0
2 returned 0
3 returned 0
4 returned 0
5 returned 0
- : unit = ()

# Fmt.pr "%a@." Uring.Stats.pp (Uring.get_debug_stats t);;
SQEs ready: 0
Operations active: 0
Sketch buffer: 0/0 (plus 0 old buffers)
- : unit = ()

# Uring.exit t;;
- : unit = ()
```

## Open

```ocaml
# let t : [ `Open ] Uring.t = Uring.create ~queue_depth:1 ();;
val t : [ `Open ] Uring.t = <abstr>
# Uring.openat2 t
                ~access:`R
                ~flags:Uring.Open_flags.empty
                ~perm:0
                ~resolve:Uring.Resolve.empty
                "/dev/null"
                `Open;;
- : [ `Open ] Uring.job option = Some <abstr>
# Uring.submit t;;;
- : int = 1

# let token, fd =
    let token, fd = consume t in
    assert (fd >= 0);
    token, (Obj.magic fd : Unix.file_descr);;
val token : [ `Open ] = `Open
val fd : Unix.file_descr = <abstr>

# Unix.read fd (Bytes.create 5) 0 5;;
- : int = 0

# let fd : unit = Unix.close fd;;
val fd : unit = ()

# Uring.exit t;;
- : unit = ()
```

## Create

```ocaml
# let t : [ `Create ] Uring.t = Uring.create ~queue_depth:1 ();;
val t : [ `Create ] Uring.t = <abstr>
# ignore @@ Unix.umask 0o077;;
- : unit = ()
# Uring.openat2 t
    ~access:`RW
    ~flags:Uring.Open_flags.creat
    ~perm:0o644
    ~resolve:Uring.Resolve.empty
    "test-openat"
    `Create;;
- : [ `Create ] Uring.job option = Some <abstr>

# Uring.submit t;;
- : int = 1

# let token, fd =
    let token, fd = consume t in
    assert (fd >= 0);
    token, (Obj.magic fd : Unix.file_descr);;
val token : [ `Create ] = `Create
val fd : Unix.file_descr = <abstr>

# Unix.write fd (Bytes.of_string "Test data") 0 9;;
- : int = 9

# let x = Unix.fstat fd in
  x.st_kind, Printf.sprintf "0o%o" x.st_perm, x.st_size;;
- : Unix.file_kind * string * int = (Unix.S_REG, "0o600", 9)

# let fd : unit = Unix.close fd;;
val fd : unit = ()

# Uring.exit t;;
- : unit = ()
```

## Statx

```ocaml
# let t : [ `Open_path | `Statx ] Uring.t = Uring.create ~queue_depth:1 ();;
val t : [ `Open_path | `Statx ] Uring.t = <abstr>

# let statx = Uring.Statx.create ();;
val statx : Uring.Statx.t = <abstr>
# Uring.statx t
    ~mask:Uring.Statx.Mask.basic_stats
    "test-openat"
    statx
    Uring.Statx.Flags.empty
    `Statx;;
- : [ `Open_path | `Statx ] Uring.job option = Some <abstr>

# Uring.submit t;;
- : int = 1

# let token, retval = consume t;;
val token : [ `Open_path | `Statx ] = `Statx
val retval : int = 0

#  Uring.Statx.kind statx, Printf.sprintf "0o%o" (Uring.Statx.perm statx), (Uring.Statx.size statx);;
- : Uring.Statx.kind * string * int64 = (`Regular_file, "0o600", 9L)

# if not (Uring.Statx.(Mask.check (mask statx) Mask.dioalign)) then assert (Uring.Statx.dio_mem_align statx = 0L);;
- : unit = ()
```

Now using `~fd`:

```ocaml
# Uring.openat2 t
    ~access:`R
    ~flags:Uring.Open_flags.path
    ~perm:0
    ~resolve:Uring.Resolve.empty
    "test-openat"
    `Open_path;;
- : [ `Open_path | `Statx ] Uring.job option = Some <abstr>

# Uring.submit t;;
- : int = 1

# let token, fd =
    let token, fd = consume t in
    assert (fd >= 0);
    token, (Obj.magic fd : Unix.file_descr);;
val token : [ `Open_path | `Statx ] = `Open_path
val fd : Unix.file_descr = <abstr>

# let statx = Uring.Statx.create ();;
val statx : Uring.Statx.t = <abstr>
# Uring.statx t
    ~fd
    ~mask:Uring.Statx.Mask.(type' + mode + size)
    ""
    statx
    Uring.Statx.Flags.empty_path
    `Statx;;
- : [ `Open_path | `Statx ] Uring.job option = Some <abstr>

# Uring.submit t;;
- : int = 1

# let token, retval = consume t;;
val token : [ `Open_path | `Statx ] = `Statx
val retval : int = 0

# Uring.Statx.kind statx, Printf.sprintf "0o%o" (Uring.Statx.perm statx), (Uring.Statx.size statx);;
- : Uring.Statx.kind * string * int64 = (`Regular_file, "0o600", 9L)

# let fd : unit = Unix.close fd;;
val fd : unit = ()

# Uring.exit t;;
- : unit = ()
```

## Resolve

```ocaml
# let t : [ `Get_path ] Uring.t = Uring.create ~queue_depth:1 ();;
val t : [ `Get_path ] Uring.t = <abstr>
# let get ~resolve path =
    assert (Option.is_some (Uring.openat2 t
                            ~access:`R
                            ~flags:Uring.Open_flags.path
                            ~perm:0
                            ~resolve
                            path
                            `Get_path));
    traceln "Submitted %d" (Uring.submit t);
    let `Get_path, fd = consume t in
    if fd >= 0 then (
      let fd : Unix.file_descr = Obj.magic fd in
      Unix.close fd;
      traceln "Opened %S OK" path
    ) else (
      raise (Unix.Unix_error (Uring.error_of_errno fd, "openat2", path))
    );;
val get : resolve:Uring.Resolve.t -> string -> unit = <fun>

# get ~resolve:Uring.Resolve.empty ".";;
Submitted 1
Opened "." OK
- : unit = ()
# get ~resolve:Uring.Resolve.beneath ".";;
Submitted 1
Opened "." OK
- : unit = ()
# get ~resolve:Uring.Resolve.empty "..";;
Submitted 1
Opened ".." OK
- : unit = ()
# get ~resolve:Uring.Resolve.beneath "..";;;
Submitted 1
Exception: Unix.Unix_error(Unix.EXDEV, "openat2", "..")

# Uring.exit t;;
- : unit = ()
```

Reading with read:

```ocaml
# let t : [`Read] Uring.t = Uring.create ~queue_depth:2 ();;
val t : [ `Read ] Uring.t = <abstr>

# let fd = Unix.openfile Test_data.path [ O_RDONLY ] 0;;
val fd : Unix.file_descr = <abstr>
# let b1_len = 3 and b2_len = 7;;
val b1_len : int = 3
val b2_len : int = 7
# let b1 = Uring.Slab.slice slab b1_len and b2 = Uring.Slab.slice slab b2_len;;
val b1 : Uring.Bstruct.t = <abstr>
val b2 : Uring.Bstruct.t = <abstr>

# Uring.read t fd b1 `Read ~file_offset:Int63.minus_one;;
- : [ `Read ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let `Read, read = consume t;;
val read : int = 3
# Uring.Bstruct.to_string b1;;
- : string = "A t"

# Uring.read t fd b2 `Read ~file_offset:Int63.minus_one;;
- : [ `Read ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let `Read, read = consume t;;
val read : int = 7
# Uring.Bstruct.to_string b2;;
- : string = "est fil"

# let fd : unit = Unix.close fd;;
val fd : unit = ()
```

Writing with write:

```ocaml
# let t : [`Read | `Write] Uring.t =  Uring.create ~queue_depth:2 ();;
val t : [ `Read | `Write ] Uring.t = <abstr>

# let rb = Uring.Slab.slice slab 10;;
val rb : Uring.Bstruct.t = <abstr>
# let wb = Uring.Slab.slice_string slab "Hello";;
val wb : Uring.Bstruct.t = <abstr>

# let r, w = Unix.pipe ();;
val r : Unix.file_descr = <abstr>
val w : Unix.file_descr = <abstr>

# Uring.write t w wb `Write ~file_offset:Int63.minus_one;;
- : [ `Read | `Write ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let v, read = consume t;;
val v : [ `Read | `Write ] = `Write
val read : int = 5

# Uring.read t r rb `Read ~file_offset:Int63.minus_one;;
- : [ `Read | `Write ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let v, read = consume t;;
val v : [ `Read | `Write ] = `Read
val read : int = 5

# Uring.Bstruct.to_string ~len:read rb;;
- : string = "Hello"

# let w : unit = Unix.close w;;
val w : unit = ()
# let r : unit = Unix.close r;;
val r : unit = ()
```
