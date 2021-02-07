(* cp(1) built with liburing. Queues up as many reads as the queue
 * depth allows and then queues up corresponding writes.
   OCaml version of https://unixism.net/loti/tutorial/cp_liburing.html *)

let queue_depth = 64
let block_size = 32*1024

let get_file_size fd =
  Unix.handle_unix_error Unix.fstat fd |>
  fun {Unix.st_size; _} -> st_size
(* TODO make this work with ST_ISBLK *)

type t = {
  mutable insize: int;
  mutable offset: int;
  mutable reads: int;
  mutable writes: int;
  mutable write_left: int;
  mutable read_left: int;
  infd: Unix.file_descr;
  outfd: Unix.file_descr;
}

type req = {
  op: [`R | `W ];
  iov: Uring.Iovec.t;
  len: int;
  mutable off: int;
  t : t;
}

let empty_req t = { op=`R; iov=Uring.Iovec.empty; len=0; off=0; t}

(* Perform a complete read into bufs. *)
let queue_read uring t len =
  let ba = Uring.Iovec.alloc_buf block_size in
  let iov = Uring.Iovec.alloc [|ba|] in
  let req = { op=`R; iov; len; off=t.offset; t } in
  Uring.readv uring t.infd iov req;
  t.offset <- t.offset + len;
  t.read_left <- t.read_left + len;
  t.reads <- t.reads + 1

(* TODO compile time check *)
let eagain = -11
let eintr = -4

(* Check that a read has completely finished, and if not
 * queue it up for completing the remaining amount *)
let handle_read_completion uring req res =
  let bytes_to_read = req.len - req.off in
  match res with
  | 0 -> raise End_of_file
  | n when n = eagain || n = eintr ->
    (* requeue the request *)
    Uring.readv ~offset:req.off uring req.t.infd req.iov req
  | n when n < 0 ->
    raise (Failure ("unix errorno " ^ (string_of_int n)))
  | n when n < bytes_to_read ->
    (* handle short read so new iovec and resubmit *)
    Uring.Iovec.advance req.iov ~idx:0 ~adj:n;
    req.off <-req.off + n;
    Uring.readv ~offset:req.off uring req.t.infd req.iov req
  | n when n = bytes_to_read ->
    (* Read is complete, all bytes are read, turn it into a write *)
    req.t.reads <- req.t.reads - 1;
    req.t.writes <- req.t.writes + 1;
    (* reset the iovec *)
    Uring.Iovec.advance req.iov ~idx:0 ~adj:(req.off * -1);
    let req = { req with op=`W; off=0 } in
    Uring.writev ~offset:0 uring req.t.outfd req.iov req
  | n -> raise (Failure ("unexpected readv result > len " ^ (string_of_int n)))

let copy_file uring t =
  (* Create a set of read requests that we will turn into write requests
   * up until the queue depth *)
  while t.write_left > 0 || t.read_left > 0 do
    let need_submit = ref false in
    let submit () =
      if t.read_left > 0 then begin
        if t.reads + t.writes < queue_depth then begin
          let size = min block_size t.read_left in
          queue_read uring t size;
          need_submit := true;
        end
      end;
      if !need_submit then
        let _ = Uring.submit uring in ()

    in
    submit ()
  done

let () =
   let infile = Sys.argv.(1) in
   let outfile = Sys.argv.(2) in
   let infd = Unix.(handle_unix_error (openfile infile [O_RDONLY]) 0) in
   let outfd = Unix.(handle_unix_error (openfile outfile [O_WRONLY; O_CREAT; O_TRUNC]) 0o644) in
   let insize = get_file_size infd in
   let t = { insize; offset=0; reads=0; writes=0; write_left=insize; read_left=insize; infd; outfd } in
   let uring = Uring.create ~queue_depth ~default:(empty_req t) () in
   copy_file uring t
   (* TOD fd close and iouring exit *)
