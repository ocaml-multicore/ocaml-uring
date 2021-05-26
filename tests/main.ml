module Int63 = Optint.Int63

let assert_      ~__POS__ = Alcotest.(check ~pos:__POS__ bool) "" true
let check_raises ~__POS__ = Alcotest.check_raises ~pos:__POS__ ""
let check_int    ~__POS__ ~expected = Alcotest.(check ~pos:__POS__ int) "" expected
let check_string ~__POS__ ~expected = Alcotest.(check ~pos:__POS__ string) "" expected

module Heap = struct
  module Heap = Uring.Private.Heap

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

  let check_raises_no_space ~__POS__:pos f =
    Alcotest.check_raises ~pos "" Heap.No_space (fun () -> ignore (f ()))

  let test_normal_usage () =
    let max_size = 10 in
    let t = Heap.create max_size in
    let reference : (Heap.ptr, int) Hashtbl.t = Hashtbl.create max_size in
    let currently_allocated = ref 0 in
    for _ = 1 to 100_000 do
      let attempt_alloc = !currently_allocated = 0 || Random.bool () in
      match attempt_alloc with
      | true ->
        if !currently_allocated = max_size then
          check_raises_no_space ~__POS__ (fun () -> Heap.alloc t 0)
        else
          let data = Random.int 5000 in
          let ptr = Heap.alloc t data in
          assert_ ~__POS__ (not (Hashtbl.mem reference ptr));
          Hashtbl.add reference ptr data;
          incr currently_allocated
      | false ->
        let (k, v) = random_hashtbl_elt reference in
        let v' = Heap.free t k in
        Hashtbl.remove reference k;
        check_int ~__POS__ ~expected:v v';
        decr currently_allocated
    done

  let test_double_free () =
    let () =
      (* Double free in an empty heap *)
      let t = Heap.create 1 in
      let p = Heap.alloc t 1 in
      check_int ~__POS__ ~expected:1 (Heap.free t p);
      check_raises ~__POS__ (Invalid_argument "Heap.free: pointer already freed")
        (fun () -> ignore (Heap.free t p))
    in
    let () =
      (* Double free in a non-empty heap *)
      let t = Heap.create 2 in
      let p = Heap.alloc t 1 in
      let _ = Heap.alloc t 2 in
      check_int ~__POS__ ~expected:1 (Heap.free t p);
      check_raises ~__POS__ (Invalid_argument "Heap.free: pointer already freed")
        (fun () -> ignore (Heap.free t p))
    in
    ()

  let test_out_of_space () =
    let () =
      let t = Heap.create 0 in
      (* 1 > 0 *)
      check_raises_no_space ~__POS__ (fun () -> Heap.alloc t ())
    in
    let () =
      let t = Heap.create 2 in
      let _ : Heap.ptr = Heap.alloc t () in
      let _ : Heap.ptr = Heap.alloc t () in
      (* 3 > 2 *)
      check_raises_no_space ~__POS__ (fun () -> Heap.alloc t ())
    in
    let () =
      let t = Heap.create 3 in
      let p1 = Heap.alloc t 1 in
      let _ : Heap.ptr = Heap.alloc t 2 in
      check_int ~__POS__ ~expected:1 (Heap.free t p1);
      let _ : Heap.ptr = Heap.alloc t 3 in
      let _ : Heap.ptr = Heap.alloc t 4 in
      (* 2 - 1 + 2 > 3 *)
      check_raises_no_space ~__POS__ (fun () -> Heap.alloc t 5);
    in
    ()
end

module Test_data = struct
  let path = "output_file.txt"

  let setup () =
    let oc = open_out path in
    output_string oc "A test file";
    close_out oc

  let with_fd f =
    let fd = Unix.openfile path [ O_RDONLY ] 0 in
    let a = f fd in
    Unix.close fd;
    a
end

let rec consume t =
  match Uring.wait ~timeout:1. t with
  | Some { data; result } -> (data, result)
  | None -> consume t

let test_invalid_queue_depth () =
  check_raises ~__POS__ (Invalid_argument "Non-positive queue depth: 0")
    (fun () -> ignore (Uring.create ~queue_depth:0 ()))

let test_noop () =
  let queue_depth = 5 in
  let t = Uring.create ~queue_depth () in

  for i = 1 to queue_depth do
    assert_ ~__POS__ (Uring.noop t i);
  done;

  check_int ~__POS__ (Uring.submit t) ~expected:queue_depth;

  for i = 1 to queue_depth do
    let tkn, res = consume t in
    check_int ~__POS__ ~expected:i tkn;
    check_int ~__POS__ ~expected:0 res
  done

let test_read () =
  let t = Uring.create ~queue_depth:1 () in
  Test_data.with_fd @@ fun fd ->

  let off = 3 in
  let len = 5 in
  let file_offset = Int63.of_int 2 in
  assert_   ~__POS__ (Uring.read t ~file_offset fd off len `Read);
  check_int ~__POS__ (Uring.submit t) ~expected:1;

  let token, read = consume t in
  assert_   ~__POS__ (token = `Read);
  check_int ~__POS__ read ~expected:len;

  let fbuf = Uring.buf t in
  check_string ~__POS__  ~expected:"test "
    (Bigstringaf.substring fbuf ~off ~len)

let test_readv () =
  let t = Uring.create ~queue_depth:1 () in
  Test_data.with_fd @@ fun fd ->

  let b1_len = 3 and b2_len = 7 in
  let b1 = Iovec.Buffer.create b1_len and b2 = Iovec.Buffer.create b2_len in
  let iov = Iovec.alloc [| b1; b2 |] in

  assert_   ~__POS__ (Uring.readv t fd iov `Readv ~file_offset:Int63.zero);
  check_int ~__POS__ (Uring.submit t) ~expected:1;

  let token, read = consume t in
  assert_      ~__POS__ (token = `Readv);
  check_int    ~__POS__ ~expected:(b1_len + b2_len) read;
  check_string ~__POS__ ~expected:"A t"     (Bigstringaf.to_string b1);
  check_string ~__POS__ ~expected:"est fil" (Bigstringaf.to_string b2);
  ()

let () =
  Test_data.setup ();
  Random.self_init ();
  let tc name f = Alcotest.test_case name `Quick f in
  Alcotest.run __FILE__ [
    "heap", [
      tc "normal_usage" Heap.test_normal_usage;
      tc "double_free" Heap.test_double_free;
      tc "out_of_space" Heap.test_out_of_space;
    ];
    "uring", [
      tc "invalid_queue_depth" test_invalid_queue_depth;
      tc "noop" test_noop;
      tc "read" test_read;
      tc "readv" test_readv;
    ];
  ]

