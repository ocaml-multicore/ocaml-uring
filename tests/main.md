```ocaml
# #require "uring";;
# #install_printer Uring.Res.pp;;
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

let consume_int ?(fn="consume_int") ?(arg="") t =
  let data, result = consume t in
  data, Uring.Res.int_exn result fn arg

let consume_fd ?(fn="consume_fd") ?(arg="") t =
  let data, result = consume t in
  data, Uring.Res.fd_exn result fn arg

let traceln fmt =
  Format.printf (fmt ^^ "@.")
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

# let fd = Unix.openfile "/dev/zero" Unix.[O_RDONLY] 0;;
val fd : Unix.file_descr = <abstr>
# let b = Cstruct.create 1;;
val b : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 1}
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
- : [ `Read ] * Uring.Res.t = (`Read, 1)
# consume t;;
- : [ `Read ] * Uring.Res.t = (`Read, 1)
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
    traceln "%d returned %a" tkn Uring.Res.pp res;
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

# let token, fd = consume_fd t;;
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

# let token, fd = consume_fd t;;
val token : [ `Create ] = `Create
val fd : Unix.file_descr = <abstr>

# Unix.write fd (Bytes.of_string "Test data") 0 9;;
- : int = 9

# let x = Unix.fstat fd in
  x.st_kind, Printf.sprintf "0o%o" x.st_perm, x.st_size;;
- : Unix.file_kind * string * int = (Unix.S_REG, "0o600", 9)

# Uring.fsync t fd `Create;;
- : [ `Create ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let v, read = consume t;;
val v : [ `Create ] = `Create
val read : Uring.Res.t = 0

# Uring.fdatasync t ~off:1L ~len:5 fd `Create;;
- : [ `Create ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let v, read = consume t;;
val v : [ `Create ] = `Create
val read : Uring.Res.t = 0

# Uring.fallocate t fd ~off:0L ~len:4096L `Create;;
- : [ `Create ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let v, read = consume t;;
val v : [ `Create ] = `Create
val read : Uring.Res.t = 0
# (Unix.fstat fd).Unix.st_size;;
- : int = 4096

# Uring.fallocate t fd ~mode:Uring.Fallocate_flags.keep_size ~off:0L ~len:8192L `Create;;
- : [ `Create ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let v, read = consume t;;
val v : [ `Create ] = `Create
val read : Uring.Res.t = 0
# (Unix.fstat fd).Unix.st_size;;
- : int = 4096

# Uring.ftruncate t fd ~len:9L `Create;;
- : [ `Create ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let v, read = consume t;;
val v : [ `Create ] = `Create
val read : Uring.Res.t = 0
# (Unix.fstat fd).Unix.st_size;;
- : int = 9

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
val retval : Uring.Res.t = 0

#  Uring.Statx.kind statx, Printf.sprintf "0o%o" (Uring.Statx.perm statx), (Uring.Statx.size statx);;
- : Uring.Statx.kind * string * int64 = (`Regular_file, "0o600", 9L)

# if not (Uring.Statx.(Mask.mem Mask.dioalign (mask statx))) then assert (Uring.Statx.dio_mem_align statx = 0L);;
- : unit = ()
# if not (Uring.Statx.(Mask.mem Mask.dioalign (mask statx))) then assert (Uring.Statx.dio_offset_align statx = 0L);;
- : unit = ()

# Int64.compare (Uring.Statx.nlink statx) 1L >= 0;;
- : bool = true
# Uring.Statx.size statx = (Unix.LargeFile.stat "test-openat").Unix.LargeFile.st_size;;
- : bool = true
# Uring.Statx.(Mask.mem Mask.size (mask statx));;
- : bool = true
# Uring.Statx.(Mask.mem Mask.type' (mask statx));;
- : bool = true
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

# let token, fd = consume_fd t;;
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
val retval : Uring.Res.t = 0

# Uring.Statx.kind statx, Printf.sprintf "0o%o" (Uring.Statx.perm statx), (Uring.Statx.size statx);;
- : Uring.Statx.kind * string * int64 = (`Regular_file, "0o600", 9L)

# let fd : unit = Unix.close fd;;
val fd : unit = ()

# Uring.exit t;;
- : unit = ()
```

### Flags, masks and attributes

Tests of the `Statx` flag handlers.

```ocaml
module S = Uring.Statx
module Attr = S.Attr
module Mask = S.Mask
module Flags = S.Flags
```

`Flags` have `+` (union), `mem` (subset) and `=` (equality) operators:

```ocaml
# assert (Flags.mem Flags.empty Flags.empty);
  assert (Flags.mem Flags.empty Flags.symlink_nofollow);
  assert (not (Flags.mem Flags.symlink_nofollow Flags.empty));;
- : unit = ()

# let combo = Flags.(symlink_nofollow + no_automount) in
  assert (Flags.mem Flags.symlink_nofollow combo);
  assert (Flags.mem Flags.no_automount combo);
  assert (not (Flags.mem Flags.empty_path combo));;
- : unit = ()

# assert Flags.(symlink_nofollow + symlink_nofollow = symlink_nofollow);
  assert Flags.(of_int (to_int symlink_nofollow) = symlink_nofollow);;
- : unit = ()
```

`Mask.basic_stats` is the union of the basic components, but excludes `btime` (birth time):

```ocaml
# let components = Mask.[ type'; mode; nlink; uid; gid; atime; mtime; ctime; ino; size; blocks ] in
  List.iter (fun c -> assert (Mask.mem c Mask.basic_stats)) components;
  assert (not (Mask.mem Mask.btime Mask.basic_stats));;
- : unit = ()

# let m = Mask.(size + type') in
  assert (Mask.mem Mask.size m);
  assert (Mask.mem Mask.type' m);
  assert (not (Mask.mem Mask.mtime m));;
- : unit = ()
```

`Attr.mem_checked ~mask attr attrs` reports whether `attr` is set in `attrs`,
treating attributes the filesystem does not support as unset.

```ocaml
let compressed = Attr.compressed
let immutable = Attr.immutable
let both = Attr.(compressed + immutable)
```

```ocaml
# (* supported and set / supported but not set *)
  assert (Attr.mem_checked ~mask:both compressed both);
  assert (not (Attr.mem_checked ~mask:both compressed immutable));;
- : unit = ()

# (* not supported -> reported as unset, rather than raising *)
  assert (not (Attr.mem_checked ~mask:immutable compressed both));
  assert (not (Attr.mem_checked ~mask:immutable compressed immutable));;
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
    let `Get_path, fd = consume_fd t ~fn:"openat2" ~arg:path in
    Unix.close fd;
    traceln "Opened %S OK" path;;
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

## Read with fixed buffer

```ocaml
let set_fixed_buffer t size =
  let fbuf = Bigarray.(Array1.create char c_layout size) in
  match Uring.set_fixed_buffer t fbuf with
  | Ok () -> fbuf
  | Error `ENOMEM -> failwith "Resource limit exceeded"

let () = Test_data.setup ()
```

```ocaml
# let t : [ `Read ] Uring.t = Uring.create ~queue_depth:1 ();;
val t : [ `Read ] Uring.t = <abstr>
# let fbuf = set_fixed_buffer t 1024;;
val fbuf :
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
  <abstr>
# let off = 3;;
val off : int = 3
# let len = 5;;
val len : int = 5
# let fd = Unix.openfile Test_data.path [ O_RDONLY ] 0;;
val fd : Unix.file_descr = <abstr>
# let file_offset = Int63.of_int 2 in
  Uring.read_fixed t ~file_offset fd ~off ~len `Read;;
- : [ `Read ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# consume t;;
- : [ `Read ] * Uring.Res.t = (`Read, 5)
# Cstruct.of_bigarray fbuf ~off ~len |> Cstruct.to_string;;
- : string = "test "

# let fd : unit = Unix.close fd;;
val fd : unit = ()
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
# let b1 = Cstruct.create b1_len and b2 = Cstruct.create b2_len;;
val b1 : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 3}
val b2 : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 7}

# Uring.read t fd b1 `Read ~file_offset:Int63.minus_one;;
- : [ `Read ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let `Read, read = consume t;;
val read : Uring.Res.t = 3
# Cstruct.to_string b1;;
- : string = "A t"

# Uring.read t fd b2 `Read ~file_offset:Int63.minus_one;;
- : [ `Read ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let `Read, read = consume t;;
val read : Uring.Res.t = 7
# Cstruct.to_string b2;;
- : string = "est fil"

# let fd : unit = Unix.close fd;;
val fd : unit = ()
```

Writing with write:

```ocaml
# let t : [`Read | `Write] Uring.t =  Uring.create ~queue_depth:2 ();;
val t : [ `Read | `Write ] Uring.t = <abstr>

# let rb = Cstruct.create 10 and wb = Cstruct.of_string "Hello";;
val rb : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 10}
val wb : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 5}
# let r, w = Unix.pipe ();;
val r : Unix.file_descr = <abstr>
val w : Unix.file_descr = <abstr>

# Uring.write t w wb `Write ~file_offset:Int63.minus_one;;
- : [ `Read | `Write ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let v, read = consume t;;
val v : [ `Read | `Write ] = `Write
val read : Uring.Res.t = 5

# Uring.read t r rb `Read ~file_offset:Int63.minus_one;;
- : [ `Read | `Write ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let v, read = consume t;;
val v : [ `Read | `Write ] = `Read
val read : Uring.Res.t = 5

# let rb = Cstruct.sub rb 0 5;;
val rb : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 5}
# Cstruct.to_string rb;;
- : string = "Hello"

# let w : unit = Unix.close w;;
val w : unit = ()
# let r : unit = Unix.close r;;
val r : unit = ()
```

Reading with readv:

```ocaml
# let t : [ `Readv ] Uring.t = Uring.create ~queue_depth:1 ();;
val t : [ `Readv ] Uring.t = <abstr>

# let fd = Unix.openfile Test_data.path [ O_RDONLY ] 0;;
val fd : Unix.file_descr = <abstr>
# let b1_len = 3 and b2_len = 7;;
val b1_len : int = 3
val b2_len : int = 7
# let b1 = Cstruct.create b1_len and b2 = Cstruct.create b2_len;;
val b1 : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 3}
val b2 : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 7}
# let iov = [b1; b2] in
  Uring.readv t fd iov `Readv ~file_offset:Int63.zero;;
- : [ `Readv ] Uring.job option = Some <abstr>

# Uring.submit t;;
- : int = 1

# let `Readv, read = consume t;;
val read : Uring.Res.t = 10
# Cstruct.to_string b1;;
- : string = "A t"
# Cstruct.to_string b2;;
- : string = "est fil"

# let fd : unit = Unix.close fd;;
val fd : unit = ()
```

Test using cstructs with offsets:

```ocaml
# let fd = Unix.openfile Test_data.path [ O_RDONLY ] 0;;
val fd : Unix.file_descr = <abstr>
# let b = Cstruct.of_string "Gathered [    ] and [   ]";;
val b : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 25}
# let b1 = Cstruct.sub b 10 4 and b2 = Cstruct.sub b 21 3 in
  let iov = [b1; b2] in
  Uring.readv t fd iov `Readv ~file_offset:Int63.zero;;
- : [ `Readv ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# consume t;;
- : [ `Readv ] * Uring.Res.t = (`Readv, 7)
# Cstruct.to_string b;;
- : string = "Gathered [A te] and [st ]"

# let fd : unit = Unix.close fd;;
val fd : unit = ()
# Uring.exit t;;
- : unit = ()
```

## Regions

```ocaml
# let t : [ `Read ] Uring.t = Uring.create ~queue_depth:1 ();;
val t : [ `Read ] Uring.t = <abstr>

# let fbuf = set_fixed_buffer t 64;;
val fbuf :
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
  <abstr>
# Uring.Region.init fbuf ~block_size:0;;
Exception: Invalid_argument "Region.init: block_size 0 must be positive".
# let region = Uring.Region.init fbuf ~block_size:16;;
val region : Uring.Region.t = <abstr>
# let chunk = Uring.Region.alloc region;;
val chunk : Uring.Region.chunk = <abstr>

# let fd = Unix.openfile Test_data.path [ O_RDONLY ] 0;;
val fd : Unix.file_descr = <abstr>
# Uring.read_chunk t fd chunk `Read ~file_offset:Int63.zero;;
- : [ `Read ] Uring.job option = Some <abstr>
# let `Read, read = consume_int t;;
val read : int = 11
# Uring.Region.to_string ~len:read chunk;;
- : string = "A test file"
# Uring.read_chunk ~len:17 t fd chunk `Read ~file_offset:Int63.zero;;
Exception:
Invalid_argument "to_cstruct: requested length 17 > block size 16".
```

Attempt to use a chunk from one ring with another:

```ocaml
# let t2 : [`Read] Uring.t = Uring.create ~queue_depth:1 ();;
val t2 : [ `Read ] Uring.t = <abstr>
# Uring.read_chunk ~len:16 t2 fd chunk `Read ~file_offset:Int63.zero;;
Exception: Invalid_argument "Chunk does not belong to ring!".

# let fd = Unix.close fd;;
val fd : unit = ()
# Uring.exit t;;
- : unit = ()
```

## Cancellation

Ask to read from a pipe (with no data available), then cancel it.

```ocaml
# exception Multiple of Uring.Res.t list;;
exception Multiple of Uring.Res.t list

# let t : [ `Cancel | `Read ] Uring.t = Uring.create ~queue_depth:5 ();;
val t : [ `Cancel | `Read ] Uring.t = <abstr>

# set_fixed_buffer t 1024;;
- : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
<abstr>
# let r, w = Unix.pipe ();;
val r : Unix.file_descr = <abstr>
val w : Unix.file_descr = <abstr>
# let read = Uring.read_fixed t ~file_offset:Int63.zero r ~off:0 ~len:1 `Read |> Option.get;;
val read : [ `Cancel | `Read ] Uring.job = <abstr>

# Uring.cancel t read `Cancel;;
- : [ `Cancel | `Read ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 2
# let t1, r1 = consume t in
  let t2, r2 = consume t in
  let r_read, r_cancel =
    match t1, t2 with
    | `Read, `Cancel -> r1, r2
    | `Cancel, `Read -> r2, r1
    | _ -> assert false
  in
  begin match Uring.Res.int_result r_read, Uring.Res.int_result r_cancel with
    | Error EINTR, Error EALREADY
      (* Occasionally, the read is actually busy just as we try to cancel.
         In that case it gets interrupted and the cancel returns EALREADY. *)
    | Error (EUNKNOWNERR 125) (* ECANCELLED *), Ok 0 ->
      (* This is the common case. The read is blocked and can just be removed. *)
      ()
    | e1, e2 -> raise (Multiple [r_read; r_cancel])
  end;;
- : unit = ()
# let r : unit = Unix.close r;;
val r : unit = ()
# let w : unit = Unix.close w;;
val w : unit = ()
# Uring.exit t;;
- : unit = ()
```

By the time we cancel, the request has already succeeded (we just didn't process the reply yet):

```ocaml
# let t : [ `Read | `Cancel ] Uring.t = Uring.create ~queue_depth:5 ();;
val t : [ `Cancel | `Read ] Uring.t = <abstr>
# set_fixed_buffer t 102;;
- : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
<abstr>
# let r = Unix.openfile "/dev/zero" Unix.[O_RDONLY] 0;;
val r : Unix.file_descr = <abstr>
# let read = Uring.read_fixed t ~file_offset:Int63.zero r ~off:0 ~len:1 `Read |> Option.get;;
val read : [ `Cancel | `Read ] Uring.job = <abstr>
# Uring.submit t;;
- : int = 1
# Unix.sleepf 0.001;;
- : unit = ()
# Uring.cancel t read `Cancel;;
- : [ `Cancel | `Read ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let t1, r1 = consume t in
  let t2, r2 = consume t in
  let r_read, r_cancel =
    match t1, t2 with
    | `Read, `Cancel -> r1, r2
    | `Cancel, `Read -> r2, r1
    | _ -> assert false
  in
  match Uring.Res.int_result r_read, Uring.Res.int_result r_cancel with
  | Ok 1, Error ENOENT -> ()
  | Ok 1, Error e -> raise (Unix.Unix_error (e, "cancel", ""))
  | Error (EUNKNOWNERR 125 (* ECANCELLED *)), Ok 0 ->
    (* This isn't the case we want to test, but it can happen sometimes. *)
    ()
  | _ -> raise (Multiple [r_read; r_cancel]);;
- : unit = ()
# let r : unit = Unix.close r;;
val r : unit = ()

# Uring.exit t;;
- : unit = ()
```

By the time we cancel, we already knew the operation was over:

```ocaml
# let t : [ `Read | `Cancel ] Uring.t = Uring.create ~queue_depth:5 ();;
val t : [ `Cancel | `Read ] Uring.t = <abstr>
# set_fixed_buffer t 1024;;
- : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
<abstr>
# let r = Unix.openfile "/dev/zero" Unix.[O_RDONLY] 0;;
val r : Unix.file_descr = <abstr>
# let read = Uring.read_fixed t ~file_offset:Int63.zero r ~off:0 ~len:1 `Read |> Option.get;;
val read : [ `Cancel | `Read ] Uring.job = <abstr>
# let token, r_read = consume t;;
val token : [ `Cancel | `Read ] = `Read
val r_read : Uring.Res.t = 1
# let r : unit = Unix.close r;;
val r : unit = ()
```

Try to cancel after we may have reused the index:
```ocaml
# Uring.cancel t read `Cancel;;
Exception: Invalid_argument "Entry has already been freed!".

# Uring.exit t;;
- : unit = ()
```

## Freeing the ring

We can't exit the ring while an operation is still pending:

```ocaml
# let t : [ `Read | `Mkdir ] Uring.t = Uring.create ~queue_depth:1 ();;
val t : [ `Mkdir | `Read ] Uring.t = <abstr>
# set_fixed_buffer t 1024;;
- : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
<abstr>
# let r, w = Unix.pipe ();;
val r : Unix.file_descr = <abstr>
val w : Unix.file_descr = <abstr>
# Uring.read_fixed t ~file_offset:Int63.minus_one r ~off:0 ~len:1 `Read;;
- : [ `Mkdir | `Read ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# Uring.exit t;;
Exception: Invalid_argument "exit: 1 request(s) still active!".
```

But we can once it's complete:

```ocaml
# let w : unit = Unix.close w;;
val w : unit = ()
# consume t;;
- : [ `Mkdir | `Read ] * Uring.Res.t = (`Read, 0)
# Uring.exit t;;
- : unit = ()
# let r : unit = Unix.close r;;
val r : unit = ()
```

We can't free the ring a second time, or use it after freeing it:

```ocaml
# Uring.unlink t ~dir:false "/doesntexist" `Mkdir;;
Exception:
Invalid_argument "Can't use ring after Uring.exit has been called".

# Uring.submit t;;
Exception:
Invalid_argument "Can't use ring after Uring.exit has been called".

# Uring.wait t;;
Exception:
Invalid_argument "Can't use ring after Uring.exit has been called".

# Uring.get_cqe_nonblocking t;;
Exception:
Invalid_argument "Can't use ring after Uring.exit has been called".

# Uring.get_probe t;;
Exception:
Invalid_argument "Can't use ring after Uring.exit has been called".

# Uring.exit t;;
Exception:
Invalid_argument "Can't use ring after Uring.exit has been called".
```

## Send_msg

```ocaml
# let r, w = Unix.pipe ();;
val r : Unix.file_descr = <abstr>
val w : Unix.file_descr = <abstr>
# let t : [ `Recv | `Send ] Uring.t= Uring.create ~queue_depth:2 ();;
val t : [ `Recv | `Send ] Uring.t = <abstr>
# let a, b = Unix.(socketpair PF_UNIX SOCK_STREAM 0);;
val a : Unix.file_descr = <abstr>
val b : Unix.file_descr = <abstr>
# let bufs = [Cstruct.of_string "hi"];;
val bufs : Cstruct.t list = [{Cstruct.buffer = <abstr>; off = 0; len = 2}]
# Uring.send_msg t a ~fds:[r; w] bufs `Send;;
- : [ `Recv | `Send ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let sent = consume t;;
val sent : [ `Recv | `Send ] * Uring.Res.t = (`Send, 2)
# let really_input_string =
    match Uring.Res.int_result (snd sent) with
    | Ok 2 -> really_input_string
    | _ -> fun _ _ -> failwith "Send failed";;
val really_input_string : in_channel -> int -> string = <fun>
# let recv_buf = Cstruct.of_string "XX";;
val recv_buf : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 2}
# let recv = Uring.Msghdr.create ~n_fds:2 [recv_buf];;
val recv : Uring.Msghdr.t = <abstr>
# List.length (Uring.Msghdr.get_fds recv);;
- : int = 0
# Uring.recv_msg t b recv `Recv;;
- : [ `Recv | `Send ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# consume t;;
- : [ `Recv | `Send ] * Uring.Res.t = (`Recv, 2)
# Cstruct.to_string recv_buf;;
- : string = "hi"
# let r2, w2 =
    match Uring.Msghdr.get_fds recv with
    | [r2; w2] -> r2, w2
    | _ -> failwith "Expected two FDs!";;
val r2 : Unix.file_descr = <abstr>
val w2 : Unix.file_descr = <abstr>
# Unix.write_substring w2 "to-w2" 0 5;;
- : int = 5
# really_input_string (Unix.in_channel_of_descr r) 5;;
- : string = "to-w2"
# Unix.write_substring w "to-w" 0 4;;
- : int = 4
# really_input_string (Unix.in_channel_of_descr r2) 4;;
- : string = "to-w"
# let r : unit = Unix.close r;;
val r : unit = ()
# let r2 : unit = Unix.close r2;;
val r2 : unit = ()
# let w2 : unit = Unix.close w2;;
val w2 : unit = ()
# let w : unit = Unix.close w;;
val w : unit = ()
# Uring.exit t;;
- : unit = ()
```

## Shutdown

```ocaml
# let t : [ `Shutdown ] Uring.t = Uring.create ~queue_depth:1 ();;
val t : [ `Shutdown ] Uring.t = <abstr>
# let a, b = Unix.(socketpair PF_UNIX SOCK_STREAM 0);;
val a : Unix.file_descr = <abstr>
val b : Unix.file_descr = <abstr>
# Uring.shutdown t a Unix.SHUTDOWN_SEND `Shutdown;;
- : [ `Shutdown ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# consume t;;
- : [ `Shutdown ] * Uring.Res.t = (`Shutdown, 0)
# let a : unit = Unix.close a;;
val a : unit = ()
# let b : unit = Unix.close b;;
val b : unit = ()
# Uring.exit t;;
- : unit = ()
```

## Socket

Create a socket on the ring and retrieve its file descriptor:

```ocaml
# let t : [ `Socket ] Uring.t = Uring.create ~queue_depth:1 ();;
val t : [ `Socket ] Uring.t = <abstr>
# Uring.socket t Unix.PF_INET Unix.SOCK_STREAM 0 `Socket;;
- : [ `Socket ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let token, fd = consume_fd t;;
val token : [ `Socket ] = `Socket
val fd : Unix.file_descr = <abstr>
```

The returned fd is a `Unix.file_descr` so connect it over loopback to a
listener and exchange a message.

```ocaml
# let roundtrip fd =
    let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Fun.protect ~finally:(fun () -> Unix.close server) @@ fun () ->
    Unix.bind server (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    Unix.listen server 1;
    Unix.connect fd (Unix.getsockname server);
    let conn, _ = Unix.accept server in
    Fun.protect ~finally:(fun () -> Unix.close conn) @@ fun () ->
    assert (Unix.write_substring conn "ping" 0 4 = 4);
    let buf = Bytes.create 4 in
    let n = Unix.read fd buf 0 4 in
    Bytes.sub_string buf 0 n;;
val roundtrip : Unix.file_descr -> string = <fun>
# roundtrip fd;;
- : string = "ping"

# let fd : unit = Unix.close fd;;
val fd : unit = ()
# Uring.exit t;;
- : unit = ()
```

## Unlink and rmdir

```ocaml
# let t : unit Uring.t = Uring.create ~queue_depth:2 ();;
val t : unit Uring.t = <abstr>

# close_out (open_out "test-file"); Unix.mkdir "test-dir" 0o700;;
- : unit = ()

# let check () = Sys.file_exists "test-file", Sys.file_exists "test-dir";;
val check : unit -> bool * bool = <fun>
# check ();;
- : bool * bool = (true, true)

# Uring.unlink t ~dir:false "test-file" ();;
- : unit Uring.job option = Some <abstr>

# Uring.unlink t ~dir:true "test-dir" ();;
- : unit Uring.job option = Some <abstr>

# Uring.wait t;;
- : unit Uring.completion_option = Uring.Some {Uring.result = 0; data = ()}

# Uring.wait t;;
- : unit Uring.completion_option = Uring.Some {Uring.result = 0; data = ()}

# check ();;
- : bool * bool = (false, false)

# Uring.exit t;;
- : unit = ()
```

## Linkat

```ocaml
# close_out (open_out "test-file");;
- : unit = ()
# Unix.symlink "test-file" "old-path";;
- : unit = ()
# let t : unit Uring.t = Uring.create ~queue_depth:2 ();;
val t : unit Uring.t = <abstr>
# Uring.linkat t ~old_path:"old-path" ~new_path:"new-symlink" ~flags:Uring.Linkat_flags.empty ();;
- : unit Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# Uring.wait t;;
- : unit Uring.completion_option = Uring.Some {Uring.result = 0; data = ()}
# (Unix.lstat "new-symlink").st_kind;;
- : Unix.file_kind = Unix.S_LNK
```

`symlink_follow` used to be broken due to <https://github.com/axboe/liburing/issues/955>
but now works.

```ocaml
# Uring.linkat t ~old_path:"old-path" ~new_path:"new-file" ~flags:Uring.Linkat_flags.symlink_follow ();;
- : unit Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# Uring.wait t;;
- : unit Uring.completion_option = Uring.Some {Uring.result = 0; data = ()}
# (Unix.lstat "new-file").st_kind;;
- : Unix.file_kind = Unix.S_REG
# ["test-file"; "old-path"; "new-symlink"; "new-file"] |> List.iter Unix.unlink;;
- : unit = ()
# Uring.exit t;;
- : unit = ()
```

## Renameat

```ocaml
# let t : unit Uring.t = Uring.create ~queue_depth:1 ();;
val t : unit Uring.t = <abstr>
# close_out (open_out "rename-src");;
- : unit = ()
# Uring.renameat t ~old_path:"rename-src" ~new_path:"rename-dst" ();;
- : unit Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# Uring.wait t;;
- : unit Uring.completion_option = Uring.Some {Uring.result = 0; data = ()}
# Sys.file_exists "rename-src", Sys.file_exists "rename-dst";;
- : bool * bool = (false, true)
```

With [noreplace], renaming onto an existing path fails with [EEXIST]:

```ocaml
# close_out (open_out "rename-src");;
- : unit = ()
# Uring.renameat t ~flags:Uring.Rename_flags.noreplace ~old_path:"rename-src" ~new_path:"rename-dst" ();;
- : unit Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# Uring.wait t;;
- : unit Uring.completion_option =
Uring.Some {Uring.result = EEXIST; data = ()}
# ["rename-src"; "rename-dst"] |> List.iter Unix.unlink;;
- : unit = ()
```

Resolving [old_path] and [new_path] against [old_dir_fd] / [new_dir_fd]
moves a file between two open directories:

```ocaml
# Unix.mkdir "rn-src-dir" 0o700; Unix.mkdir "rn-dst-dir" 0o700;;
- : unit = ()
# close_out (open_out "rn-src-dir/a");;
- : unit = ()
# let src_dir = Unix.openfile "rn-src-dir" [ O_RDONLY ] 0;;
val src_dir : Unix.file_descr = <abstr>
# let dst_dir = Unix.openfile "rn-dst-dir" [ O_RDONLY ] 0;;
val dst_dir : Unix.file_descr = <abstr>
# Uring.renameat t ~old_dir_fd:src_dir ~new_dir_fd:dst_dir
    ~old_path:"a" ~new_path:"b" ();;
- : unit Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# Uring.wait t;;
- : unit Uring.completion_option = Uring.Some {Uring.result = 0; data = ()}
# Sys.file_exists "rn-src-dir/a", Sys.file_exists "rn-dst-dir/b";;
- : bool * bool = (false, true)
# let src_dir : unit = Unix.close src_dir;;
val src_dir : unit = ()
# let dst_dir : unit = Unix.close dst_dir;;
val dst_dir : unit = ()
# Unix.unlink "rn-dst-dir/b";;
- : unit = ()
# ["rn-src-dir"; "rn-dst-dir"] |> List.iter Unix.rmdir;;
- : unit = ()
# Uring.exit t;;
- : unit = ()
```

## Symlinkat

```ocaml
# let t : unit Uring.t = Uring.create ~queue_depth:1 ();;
val t : unit Uring.t = <abstr>
# Uring.symlinkat t ~target:"the-target" ~link_path:"the-link" ();;
- : unit Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# Uring.wait t;;
- : unit Uring.completion_option = Uring.Some {Uring.result = 0; data = ()}
# (Unix.lstat "the-link").st_kind;;
- : Unix.file_kind = Unix.S_LNK
# Unix.readlink "the-link";;
- : string = "the-target"
# Unix.unlink "the-link";;
- : unit = ()
```

With [dir_fd], [link_path] is resolved against an open directory (note that
[target] is stored verbatim and is never resolved against [dir_fd]):

```ocaml
# Unix.mkdir "sl-dir" 0o700;;
- : unit = ()
# let dir = Unix.openfile "sl-dir" [ O_RDONLY ] 0;;
val dir : Unix.file_descr = <abstr>
# Uring.symlinkat t ~target:"the-target" ~dir_fd:dir ~link_path:"inner-link" ();;
- : unit Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# Uring.wait t;;
- : unit Uring.completion_option = Uring.Some {Uring.result = 0; data = ()}
# (Unix.lstat "sl-dir/inner-link").st_kind;;
- : Unix.file_kind = Unix.S_LNK
# Unix.readlink "sl-dir/inner-link";;
- : string = "the-target"
# let dir : unit = Unix.close dir;;
val dir : unit = ()
# Unix.unlink "sl-dir/inner-link"; Unix.rmdir "sl-dir";;
- : unit = ()
# Uring.exit t;;
- : unit = ()
```

## Mkdirat

```ocaml
# let t : [ `Mkdir of int ] Uring.t = Uring.create ~queue_depth:1 ();;
val t : [ `Mkdir of int ] Uring.t = <abstr>
# Uring.mkdirat t ~mode:0o755 "mkdir" (`Mkdir 0);;
- : [ `Mkdir of int ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# Uring.wait t;;
- : [ `Mkdir of int ] Uring.completion_option =
Uring.Some {Uring.result = 0; data = `Mkdir 0}
# Printf.sprintf "0o%o" ((Unix.stat "mkdir").st_perm land 0o777);;
- : string = "0o700"
# let v = Uring.mkdirat t ~mode:0o755 "mkdir" (`Mkdir 1);;
val v : [ `Mkdir of int ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# Uring.wait t;;
- : [ `Mkdir of int ] Uring.completion_option =
Uring.Some {Uring.result = EEXIST; data = `Mkdir 1}
# Uring.exit t;;
- : unit = ()
```

## Timeout

Timeout should return (-ETIME). This is defined in https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/errno.h#L45

```ocaml
# let t : [`Timeout] Uring.t = Uring.create ~queue_depth:1 ();;
val t : [ `Timeout ] Uring.t = <abstr>

# let ns1 = Int64.(mul 10L 1_000_000L) in
  Uring.(timeout t Boottime ns1 `Timeout);;
- : [ `Timeout ] Uring.job option = Some <abstr>

# Uring.submit t;;
- : int = 1

# let `Timeout, timeout = consume t;;
val timeout : Uring.Res.t = ETIME

# let ns = 
    ((Unix.gettimeofday () +. 0.01) *. 1e9)
    |> Int64.of_float
  in
  Uring.(timeout ~absolute:true t Realtime ns `Timeout);;
- : [ `Timeout ] Uring.job option = Some <abstr>

# let `Timeout, timeout = consume t;;
val timeout : Uring.Res.t = ETIME

# let ns1 = Int64.(mul 10L 1_000_000L) in
  Uring.(timeout ~absolute:true t Boottime ns1 `Timeout);;
- : [ `Timeout ] Uring.job option = Some <abstr>

# let `Timeout, timeout = consume t;;
val timeout : Uring.Res.t = ETIME

# Uring.exit t;;
- : unit = ()
```

If there is a timeout but we did submit something, `io_uring_submit_and_wait_timeout` returns success instead:

```ocaml
# let t : [`Timeout | `Cancel] Uring.t = Uring.create ~queue_depth:1 ();;
val t : [ `Cancel | `Timeout ] Uring.t = <abstr>

# let job =
    let ns = Int64.(mul 10L 1_000_000_000L) in
    Uring.(timeout t Boottime ns `Timeout);;
val job : [ `Cancel | `Timeout ] Uring.job option = Some <abstr>

# Uring.wait ~timeout:0.01 t;;Uring.wait ~timeout:0.01 t;;
- : [ `Cancel | `Timeout ] Uring.completion_option = Uring.None

# Uring.cancel t (Option.get job) `Cancel;;
- : [ `Cancel | `Timeout ] Uring.job option = Some <abstr>

# ignore (Uring.wait ~timeout:10.0 t, Uring.wait ~timeout:10.0 t);;
- : unit = ()

# Uring.exit t;;
- : unit = ()
```


## Probing

```ocaml
# let t : unit Uring.t = Uring.create ~queue_depth:1 ();;
val t : unit Uring.t = <abstr>

# let probe = Uring.get_probe t in
  Uring.op_supported probe Uring.Op.nop;;
- : bool = true

# Uring.exit t;;
- : unit = ()
```
