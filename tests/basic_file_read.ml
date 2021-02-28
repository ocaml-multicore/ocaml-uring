(* TODO also write test.txt *)

let () =
  let t = Uring.create ~queue_depth:1 ~default:() () in
  let fd = Unix.(handle_unix_error (openfile "test.txt" [O_RDONLY]) 0) in
  let b1 = Uring.Iovec.alloc_buf 3 in
  let b2 = Uring.Iovec.alloc_buf 7 in
  let iov = Uring.Iovec.alloc [|b1;b2|] in
  let r = Uring.readv t fd iov () in assert(r);
  let res = Uring.submit t in
  Printf.eprintf "submitted %d\n%!" res;
  let rec retry () =
    match Uring.wait t with
    | None -> retry ()
    | Some v -> v
  in
  let (), res = retry () in
  Uring.Iovec.free iov;
  Printf.eprintf "res %d\n%!" res;
  Printf.eprintf "%s -- %s\n%!" (Bigstringaf.to_string b1) (Bigstringaf.to_string b2);
  let off = 3 in
  let len = 5 in
  let file_offset = 2 in
  let r = Uring.read t ~file_offset fd off len () in assert(r);
  let res = Uring.submit t in
  Printf.eprintf "submitted read %d\n%!" res;
  let (), res = retry () in
  Printf.eprintf "res %d\n%!" res;
  let fbuf = Uring.buf t in
  Printf.eprintf "read: '%s'\n%!" (Bigstringaf.substring fbuf ~off ~len);
  ()
