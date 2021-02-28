(* cat(1) built with liburing.
   OCaml version of https://unixism.net/loti/tutorial/cat_liburing.html *)

let block_size = 1024

let get_file_size fd =
  Unix.handle_unix_error Unix.fstat fd |>
  fun {Unix.st_size; _} -> st_size
(* TODO make this work with ST_ISBLK *)

let get_completion_and_print uring =
  let iov, len = match Uring.wait uring with Some v -> v | None -> failwith "retry" in
  let bufs = Uring.Iovec.bufs iov in
  let remaining = ref len in
  Printf.eprintf "%d bytes read\n%!" len;
  Array.iter (fun buf ->
    let buflen = Bigstringaf.length buf in
    if !remaining > 0 then begin
      if buflen <= !remaining then begin
        print_string (Bigstringaf.to_string buf);
        remaining := !remaining - buflen;
      end else begin
        print_string (Bigstringaf.substring ~off:0 ~len:!remaining buf);
        remaining := 0;
      end
    end
  ) bufs

let submit_read_request fname uring =
  let fd = Unix.(handle_unix_error (openfile fname [O_RDONLY]) 0) in
  let file_sz = get_file_size fd in
  let blocks = if file_sz mod block_size <> 0 then (file_sz / block_size)+1 else file_sz/block_size in
  let bufs = Array.init blocks (fun _ -> Uring.Iovec.alloc_buf block_size) in
  let iov = Uring.Iovec.alloc bufs in
  let _ = Uring.readv uring fd iov (iov :> Uring.Iovec.t) in
  let numreq = Uring.submit uring in
  assert(numreq=1);
  ()

let () =
   let fname = Sys.argv.(1) in
   let uring = Uring.create ~queue_depth:1 ~default:Uring.Iovec.empty () in
   submit_read_request fname uring;
   get_completion_and_print uring
