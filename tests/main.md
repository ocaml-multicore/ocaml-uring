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
```

## Invalid queue depth

```ocaml
# Uring.create ~queue_depth:0 ();;
Exception: Invalid_argument "Non-positive queue depth: 0".
```

## Noop

```ocaml
# let queue_depth = 5;;
val queue_depth : int = 5

# let t = Uring.create ~queue_depth ();;
val t : '_weak1 Uring.t = <abstr>

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
# let t = Uring.create ~queue_depth:1 ();;
val t : '_weak2 Uring.t = <abstr>
# Uring.openat2 t
                ~access:`R
                ~flags:Uring.Open_flags.empty
                ~perm:0
                ~resolve:Uring.Resolve.empty
                "/dev/null"
                `Open;;
- : _[> `Open ] Uring.job option = Some <abstr>
# Uring.submit t;;;
- : int = 1

# let token, fd =
    let token, fd = consume t in
    assert (fd >= 0);
    token, (Obj.magic fd : Unix.file_descr);;
val token : _[> `Open ] = `Open
val fd : Unix.file_descr = <abstr>

# Unix.read fd (Bytes.create 5) 0 5;;
- : int = 0

# Unix.close fd;;
- : unit = ()

# Uring.exit t;;
- : unit = ()
```

## Create

```ocaml
# let t = Uring.create ~queue_depth:1 ();;
val t : '_weak3 Uring.t = <abstr>
# ignore @@ Unix.umask 0o077;;
- : unit = ()
# Uring.openat2 t
    ~access:`RW
    ~flags:Uring.Open_flags.creat
    ~perm:0o644
    ~resolve:Uring.Resolve.empty
    "test-openat"
    `Create;;
- : _[> `Create ] Uring.job option = Some <abstr>

# Uring.submit t;;
- : int = 1

# let token, fd =
    let token, fd = consume t in
    assert (fd >= 0);
    token, (Obj.magic fd : Unix.file_descr);;
val token : _[> `Create ] = `Create
val fd : Unix.file_descr = <abstr>

# Unix.write fd (Bytes.of_string "Test") 0 4;;
- : int = 4
# Printf.sprintf "0o%o" (Unix.fstat fd).st_perm;;
- : string = "0o600"

# Unix.close fd;;
- : unit = ()

# Uring.exit t;;
- : unit = ()
```

## Resolve

```ocaml
# let t = Uring.create ~queue_depth:1 ();;
val t : '_weak4 Uring.t = <abstr>
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
# let t = Uring.create ~queue_depth:1 ();;
val t : '_weak5 Uring.t = <abstr>
# let fbuf = set_fixed_buffer t 1024;;
val fbuf :
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
  <abstr>
# let off = 3;;
val off : int = 3
# let len = 5;;
val len : int = 5
# let fd = Unix.openfile Test_data.path [ O_RDONLY ] 0 in
  let file_offset = Int63.of_int 2 in
  Uring.read_fixed t ~file_offset fd ~off ~len `Read;;
- : _[> `Read ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# consume t;;
- : _[> `Read ] * int = (`Read, 5)
# Cstruct.of_bigarray fbuf ~off ~len |> Cstruct.to_string;;
- : string = "test "

# Unix.close fd; Uring.exit t;;
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
val read : int = 3
# Cstruct.to_string b1;;
- : string = "A t"

# Uring.read t fd b2 `Read ~file_offset:Int63.minus_one;;
- : [ `Read ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let `Read, read = consume t;;
val read : int = 7
# Cstruct.to_string b2;;
- : string = "est fil"

# Unix.close fd;;
- : unit = ()
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
val read : int = 5

# Uring.read t r rb `Read ~file_offset:Int63.minus_one;;
- : [ `Read | `Write ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# let v, read = consume t;;
val v : [ `Read | `Write ] = `Read
val read : int = 5

# let rb = Cstruct.sub rb 0 5;;
val rb : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 5}
# Cstruct.to_string rb;;
- : string = "Hello"

# Unix.close w;;
- : unit = ()
# Unix.close r;;
- : unit = ()
```

Reading with readv:

```ocaml
# let t = Uring.create ~queue_depth:1 ();;
val t : '_weak6 Uring.t = <abstr>

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
- : _[> `Readv ] Uring.job option = Some <abstr>

# Uring.submit t;;
- : int = 1

# let `Readv, read = consume t;;
val read : int = 10
# Cstruct.to_string b1;;
- : string = "A t"
# Cstruct.to_string b2;;
- : string = "est fil"

# Unix.close fd;;
- : unit = ()
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
- : [ `Readv ] * int = (`Readv, 7)
# Cstruct.to_string b;;
- : string = "Gathered [A te] and [st ]"

# Unix.close fd; Uring.exit t;;
- : unit = ()
```

## Regions

```ocaml
# let t = Uring.create ~queue_depth:1 ();;
val t : '_weak7 Uring.t = <abstr>

# let fbuf = set_fixed_buffer t 64;;
val fbuf :
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
  <abstr>
# let region = Uring.Region.init fbuf 4 ~block_size:16;;
val region : Uring.Region.t = <abstr>
# let chunk = Uring.Region.alloc region;;
val chunk : Uring.Region.chunk = <abstr>

# let fd = Unix.openfile Test_data.path [ O_RDONLY ] 0;;
val fd : Unix.file_descr = <abstr>
# Uring.read_chunk t fd chunk `Read ~file_offset:Int63.zero;;
- : _[> `Read ] Uring.job option = Some <abstr>
# let `Read, read = consume t;;
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

# Unix.close fd; Uring.exit t;;
- : unit = ()
```

## Cancellation

Ask to read from a pipe (with no data available), then cancel it.

```ocaml
# exception Multiple of Unix.error list;;
exception Multiple of Unix.error list

# let t = Uring.create ~queue_depth:5 ();;
val t : '_weak8 Uring.t = <abstr>

# set_fixed_buffer t 1024;;
- : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
<abstr>
# let r, w = Unix.pipe ();;
val r : Unix.file_descr = <abstr>
val w : Unix.file_descr = <abstr>
# let read = Uring.read_fixed t ~file_offset:Int63.zero r ~off:0 ~len:1 `Read |> Option.get;;
val read : _[> `Read ] Uring.job = <abstr>

# Uring.cancel t read `Cancel;;
- : _[> `Cancel | `Read ] Uring.job option = Some <abstr>
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
  begin match Uring.error_of_errno r_read, Uring.error_of_errno r_cancel with
    | EINTR, EALREADY
      (* Occasionally, the read is actually busy just as we try to cancel.
         In that case it gets interrupted and the cancel returns EALREADY. *)
    | EUNKNOWNERR 125 (* ECANCELLED *), EUNKNOWNERR 0 ->
      (* This is the common case. The read is blocked and can just be removed. *)
      ()
    | e1, e2 -> raise (Multiple [e1; e2])
  end;
  Unix.close r;
  Unix.close w;
  (* done *)
  Uring.exit t;;
- : unit = ()
```

By the time we cancel, the request has already succeeded (we just didn't process the reply yet):

```ocaml
# let t = Uring.create ~queue_depth:5 ();;
val t : '_weak9 Uring.t = <abstr>
# set_fixed_buffer t 102;;
- : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
<abstr>
# let r = Unix.openfile "/dev/zero" Unix.[O_RDONLY] 0;;
val r : Unix.file_descr = <abstr>
# let read = Uring.read_fixed t ~file_offset:Int63.zero r ~off:0 ~len:1 `Read |> Option.get;;
val read : _[> `Read ] Uring.job = <abstr>
# Uring.submit t;;
- : int = 1
# Unix.sleepf 0.001;;
- : unit = ()
# Uring.cancel t read `Cancel;;
- : _[> `Cancel | `Read ] Uring.job option = Some <abstr>
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
  if r_read = 1 then (
    match Uring.error_of_errno r_cancel with
    | ENOENT -> ()
    | e -> raise (Unix.Unix_error (e, "cancel", ""))
  ) else (
    match Uring.error_of_errno r_read, Uring.error_of_errno r_cancel with
    | EUNKNOWNERR 125 (* ECANCELLED *), EUNKNOWNERR 0 ->
      (* This isn't the case we want to test, but it can happen sometimes. *)
      ()
    | e1, e2 -> raise (Multiple [e1; e2])
  );;
- : unit = ()
# Unix.close r;;
- : unit = ()

# Uring.exit t;;
- : unit = ()
```

By the time we cancel, we already knew the operation was over:

```ocaml
# let t = Uring.create ~queue_depth:5 ();;
val t : '_weak10 Uring.t = <abstr>
# set_fixed_buffer t 1024;;
- : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
<abstr>
# let r = Unix.openfile "/dev/zero" Unix.[O_RDONLY] 0;;
val r : Unix.file_descr = <abstr>
# let read = Uring.read_fixed t ~file_offset:Int63.zero r ~off:0 ~len:1 `Read |> Option.get;;
val read : _[> `Read ] Uring.job = <abstr>
# let token, r_read = consume t;;
val token : _[> `Read ] = `Read
val r_read : int = 1
# Unix.close r;;
- : unit = ()
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
# let t = Uring.create ~queue_depth:1 ();;
val t : '_weak11 Uring.t = <abstr>
# set_fixed_buffer t 1024;;
- : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
<abstr>
# let r, w = Unix.pipe ();;
val r : Unix.file_descr = <abstr>
val w : Unix.file_descr = <abstr>
# Uring.read_fixed t ~file_offset:Int63.minus_one r ~off:0 ~len:1 `Read;;
- : _[> `Read ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# Uring.exit t;;
Exception: Invalid_argument "exit: 1 request(s) still active!".
```

But we can once it's complete:

```ocaml
# Unix.close w;;
- : unit = ()
# consume t;;
- : _[> `Read ] * int = (`Read, 0)
# Uring.exit t;;
- : unit = ()
# Unix.close r;;
- : unit = ()
```

## Send_msg

```ocaml
# let r, w = Unix.pipe ();;
val r : Unix.file_descr = <abstr>
val w : Unix.file_descr = <abstr>
# let t = Uring.create ~queue_depth:2 ();;
val t : '_weak12 Uring.t = <abstr>
# let a, b = Unix.(socketpair PF_UNIX SOCK_STREAM 0);;
val a : Unix.file_descr = <abstr>
val b : Unix.file_descr = <abstr>
# let bufs = [Cstruct.of_string "hi"];;
val bufs : Cstruct.t list = [{Cstruct.buffer = <abstr>; off = 0; len = 2}]
# Uring.send_msg t a ~fds:[r; w] bufs `Send;;
- : _[> `Send ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# consume t;;
- : _[> `Send ] * int = (`Send, 2)
# let recv_buf = Cstruct.of_string "XX";;
val recv_buf : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 2}
# let recv = Uring.Msghdr.create ~n_fds:2 [recv_buf];;
val recv : Uring.Msghdr.t = <abstr>
# List.length (Uring.Msghdr.get_fds recv);;
- : int = 0
# Uring.recv_msg t b recv `Recv;;
- : _[> `Recv | `Send ] Uring.job option = Some <abstr>
# Uring.submit t;;
- : int = 1
# consume t;;
- : _[> `Recv | `Send ] * int = (`Recv, 2)
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
# List.iter Unix.close [r; w; r2; w2];;
- : unit = ()
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

## Timeout

Timeout should return (-ETIME). This is defined in https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/errno.h#L45

```ocaml
# let t = Uring.create ~queue_depth:1 ();;
val t : '_weak13 Uring.t = <abstr>

# let ns1 = Int64.(mul 10L 1_000_000L) in
  Uring.(timeout t Boottime ns1 `Timeout);;
- : _[> `Timeout ] Uring.job option = Some <abstr>

# Uring.submit t;;
- : int = 1

# let `Timeout, timeout = consume t;;
val timeout : int = -62

# let ns = 
    ((Unix.gettimeofday () +. 0.01) *. 1e9)
    |> Int64.of_float
  in
  Uring.(timeout ~absolute:true t Realtime ns `Timeout);;
- : [ `Timeout ] Uring.job option = Some <abstr>

# let `Timeout, timeout = consume t;;
val timeout : int = -62

# let ns1 = Int64.(mul 10L 1_000_000L) in
  Uring.(timeout ~absolute:true t Boottime ns1 `Timeout);;
- : [ `Timeout ] Uring.job option = Some <abstr>

# let `Timeout, timeout = consume t;;
val timeout : int = -62

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
