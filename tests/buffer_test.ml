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

let () =
  test_immovable_under_gc ();
  test_fixed_buffer ()
