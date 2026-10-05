(* Tests that io_uring I/O works correctly with plain immovable [bytes]:
   that a buffer survives GC compaction while the kernel holds its pointer, and
   that the optional pre-registered fixed buffer behaves. *)

module Int63 = Optint.Int63

let check name b = if not b then failwith ("assertion failed: " ^ name)

let raises f = try ignore (f ()); false with Invalid_argument _ -> true

(* End-to-end: the buffer must survive a GC compaction while the kernel still
   holds its raw pointer, proving a [>= min_buffer_size] bytes is immovable. *)
let test_immovable_under_gc () =
  check "min_buffer_size" (Uring.min_buffer_size = 2048);
  let t = Uring.create ~queue_depth:8 () in
  let r, w = Unix.pipe () in
  let msg = String.init 4096 (fun i -> Char.chr (i land 0xff)) in
  assert (Unix.write_substring w msg 0 (String.length msg) = String.length msg);
  let buf = Bytes.create 4096 in
  let iov = Uring.Iovec.of_bytes buf in
  let _job = Uring.read t r iov `Read ~file_offset:Int63.minus_one |> Option.get in
  assert (Uring.submit t = 1);
  (* Churn and compact the heap while the read is in flight. If [buf] could be
     relocated, the kernel would scribble on the old location. *)
  for _ = 1 to 5 do
    ignore (Sys.opaque_identity (Array.init 10000 (fun i -> Bytes.create (i land 255))));
    Gc.compact ()
  done;
  let got =
    match Uring.wait t with
    | Some { result; _ } -> Uring.Res.int_exn result "read" ""
    | None -> failwith "no completion"
  in
  check "read length" (got = String.length msg);
  check "read data intact after compaction" (Bytes.sub_string buf 0 got = msg);
  Unix.close r;
  Unix.close w;
  Uring.exit t;
  print_endline "immovability-under-GC test passed"

(* The opt-in fixed buffer registered by [create ~fixed_buffer_size], and that
   the default ring has none. *)
let test_fixed_buffer () =
  let t = Uring.create ~queue_depth:4 ~fixed_buffer_size:(64 * 1024) () in
  (match Uring.buf t with
   | Some b -> check "buf size" (Bytes.length b = 64 * 1024)
   | None -> check "buf present" false);
  let r, w = Unix.pipe () in
  let msg = "fixed-buffer hello" in
  assert (Unix.write_substring w msg 0 (String.length msg) = String.length msg);
  let _job = Uring.read_fixed t ~file_offset:Int63.minus_one r ~off:0 ~len:64 `R |> Option.get in
  assert (Uring.submit t = 1);
  let got =
    match Uring.wait t with
    | Some { result; _ } -> Uring.Res.int_exn result "read_fixed" ""
    | None -> failwith "no completion"
  in
  check "fixed read len" (got = String.length msg);
  (match Uring.buf t with
   | Some b -> check "fixed read data" (Bytes.sub_string b 0 got = msg)
   | None -> check "buf still present" false);
  Unix.close r;
  Unix.close w;
  Uring.exit t;
  (* The default ring has no fixed buffer, and read_fixed rejects that clearly. *)
  let t2 = Uring.create ~queue_depth:1 () in
  check "no fixed buffer" (Uring.buf t2 = None);
  check "read_fixed without buffer raises"
    (raises (fun () -> Uring.read_fixed t2 ~file_offset:Int63.zero Unix.stdin ~off:0 ~len:1 `R));
  Uring.exit t2;
  print_endline "fixed-buffer test passed"

(* [Iovec.to_bigarray] aliases the live region so writes on either side are
   visible to the other, and [off]/[len] are bounds checked. *)
let test_to_bigarray_aliasing () =
  let buf = Bytes.make 4096 '.' in
  let iov = Uring.Iovec.of_bytes ~off:100 ~len:200 buf in
  let ba = Uring.Iovec.to_bigarray iov in
  check "ba length" (Bigarray.Array1.dim ba = 200);
  (* bytes -> bigarray *)
  Bytes.set buf 100 'a';
  Bytes.set buf 299 'z';
  check "bytes write visible at ba.(0)" (Bigarray.Array1.get ba 0 = 'a');
  check "bytes write visible at ba.(199)" (Bigarray.Array1.get ba 199 = 'z');
  (* bigarray -> bytes *)
  Bigarray.Array1.set ba 1 'B';
  check "ba write visible in bytes" (Bytes.get buf 101 = 'B');
  check "ba write did not leak outside region"
    (Bytes.get buf 99 = '.' && Bytes.get buf 300 = '.');
  print_endline "to_bigarray aliasing test passed"

(* A kernel read completed into the iovec must be visible, zero-copy, through
   a bigarray alias taken before submission. *)
let test_to_bigarray_kernel_read () =
  let t = Uring.create ~queue_depth:4 () in
  let r, w = Unix.pipe () in
  let msg = String.init 3000 (fun i -> Char.chr ((i * 7) land 0xff)) in
  assert (Unix.write_substring w msg 0 (String.length msg) = String.length msg);
  let iov = Uring.Iovec.create 4096 in
  let ba = Uring.Iovec.to_bigarray iov in
  let _job = Uring.read t r iov `Read ~file_offset:Int63.minus_one |> Option.get in
  assert (Uring.submit t = 1);
  let got =
    match Uring.wait t with
    | Some { result; _ } -> Uring.Res.int_exn result "read" ""
    | None -> failwith "no completion"
  in
  check "read length" (got = String.length msg);
  let via_ba = String.init got (Bigarray.Array1.get ba) in
  check "kernel read visible through bigarray" (via_ba = msg);
  Unix.close r;
  Unix.close w;
  Uring.exit t;
  print_endline "to_bigarray kernel-read test passed"

(* The bigarray keeps the backing bytes alive after the iovec is dropped. *)
let test_to_bigarray_keep_alive () =
  let make () =
    let iov = Uring.Iovec.create 4096 in
    Bytes.blit_string "keep-alive" 0 iov.Uring.Iovec.buf 0 10;
    Uring.Iovec.to_bigarray iov
    (* [iov] and its bytes go out of scope here; only [ba] roots the memory. *)
  in
  let ba = (make [@inlined never]) () in
  for _ = 1 to 5 do
    ignore (Sys.opaque_identity (Array.init 10000 (fun i -> Bytes.create (i land 255))));
    Gc.full_major ();
    Gc.compact ()
  done;
  let s = String.init 10 (Bigarray.Array1.get ba) in
  check "contents intact after GC churn" (s = "keep-alive");
  print_endline "to_bigarray keep-alive test passed"

(* An [Array1.sub] of the bigarray shares the runtime proxy, so it keeps the
   backing bytes alive even after the iovec and the original bigarray are
   both unreachable. *)
let test_to_bigarray_sub_keep_alive () =
  let make () =
    let iov = Uring.Iovec.create 4096 in
    Bytes.blit_string "0123456789" 0 iov.Uring.Iovec.buf 0 10;
    let ba = Uring.Iovec.to_bigarray iov in
    Bigarray.Array1.sub ba 2 5
    (* [iov] and [ba] go out of scope; only the sub roots the memory. *)
  in
  let sub = (make [@inlined never]) () in
  for _ = 1 to 5 do
    ignore (Sys.opaque_identity (Array.init 10000 (fun i -> Bytes.create (i land 255))));
    Gc.full_major ();
    Gc.compact ()
  done;
  let s = String.init 5 (Bigarray.Array1.get sub) in
  check "sub contents intact after GC churn" (s = "23456");
  print_endline "to_bigarray sub keep-alive test passed"

(* Once every bigarray of a family is collected, the refcount drops so its
   collectable. Use a weak point to check that here. *)
let test_alias_release () =
  let w = Weak.create 1 in
  let make () =
    let iov = Uring.Iovec.create 4096 in
    Weak.set w 0 (Some iov.Uring.Iovec.buf);
    let ba = Uring.Iovec.to_bigarray iov in
    Bigarray.Array1.sub ba 0 8
    (* [iov] and [ba] go out of scope; only the sub remains. *)
  in
  let sub = (make [@inlined never]) () in
  for _ = 1 to 5 do Gc.full_major () done;
  check "bytes alive while sub is live" (Weak.get w 0 <> None);
  check "sub readable" (Bigarray.Array1.dim sub = 8);
  ignore (Sys.opaque_identity sub);
  (* Now the sub is dead too: the family collapses and the bytes must go. *)
  let rec wait n =
    n > 0 && begin
      Gc.full_major ();
      Weak.get w 0 = None || wait (n - 1)
    end
  in
  check "bytes released after family collected" (wait 10);
  print_endline "alias release test passed"

let () =
  test_immovable_under_gc ();
  test_fixed_buffer ();
  test_to_bigarray_aliasing ();
  test_to_bigarray_kernel_read ();
  test_to_bigarray_keep_alive ();
  test_to_bigarray_sub_keep_alive ();
  test_alias_release ()
