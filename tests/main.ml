let assert_      ~__POS__ = Alcotest.(check ~pos:__POS__ bool) "" true
let check_int    ~__POS__ ~expected = Alcotest.(check ~pos:__POS__ int) "" expected
let check_string ~__POS__ ~expected = Alcotest.(check ~pos:__POS__ string) "" expected

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
  | Some v -> v
  | None -> consume t

let test_invalid_queue_depth () =
  Alcotest.check_raises "" (Invalid_argument "Non-positive queue depth: 0")
    (fun () -> ignore (Uring.create ~queue_depth:0 ~default:() ()))

let test_noop () =
  let queue_depth = 5 in
  let t = Uring.create ~queue_depth ~default:0 () in

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
  let t = Uring.create ~queue_depth:1 ~default:`Unused () in
  Test_data.with_fd @@ fun fd ->

  let off = 3 in
  let len = 5 in
  let file_offset = 2 in
  assert_   ~__POS__ (Uring.read t ~file_offset fd off len `Read);
  check_int ~__POS__ (Uring.submit t) ~expected:1;

  let token, read = consume t in
  assert_   ~__POS__ (token = `Read);
  check_int ~__POS__ read ~expected:len;

  let fbuf = Uring.buf t in
  check_string ~__POS__  ~expected:"test "
    (Bigstringaf.substring fbuf ~off ~len)

let test_readv () =
  let t = Uring.create ~queue_depth:1 ~default:`Unused () in
  Test_data.with_fd @@ fun fd ->

  let b1_len = 3 and b2_len = 7 in
  let b1 = Iovec.Buffer.create b1_len and b2 = Iovec.Buffer.create b2_len in
  let iov = Iovec.alloc [| b1; b2 |] in

  assert_   ~__POS__ (Uring.readv t fd iov `Readv);
  check_int ~__POS__ (Uring.submit t) ~expected:1;

  let token, read = consume t in
  assert_      ~__POS__ (token = `Readv);
  check_int    ~__POS__ ~expected:(b1_len + b2_len) read;
  check_string ~__POS__ ~expected:"A t"     (Bigstringaf.to_string b1);
  check_string ~__POS__ ~expected:"est fil" (Bigstringaf.to_string b2);
  ()

let () =
  Test_data.setup ();
  let tc name f = Alcotest.test_case name `Quick f in
  Alcotest.run __FILE__ [ "uring", [
      tc "invalid_queue_depth" test_invalid_queue_depth;
      tc "noop" test_noop;
      tc "read" test_read;
      tc "readv" test_readv;
    ] ]

