(* cp(1) built with Lwt. *)
open Lwt.Infix

let full_io iofn fd buf ~file_offset off len =
  let rec fn off =
    let file_offset = file_offset + off in
    iofn fd buf ~file_offset off (len - off) >>= function
    | 0 -> if off < len then Lwt.fail End_of_file else Lwt.return_unit
    | n when n < len - off -> fn (off+n)
    | _n -> Lwt.return_unit
   in
   fn off

let full_pread = full_io Lwt_unix.pread
let full_pwrite = full_io Lwt_unix.pwrite

let copy_file infd outfd insize block_size pool =
  let reqs = ref [] in
  let read_then_write_chunk ~file_offset len =
    Lwt_pool.use pool (fun buf ->
      full_pread infd buf ~file_offset 0 len >>= fun () ->
      Logs.debug (fun l -> l "read done %d %d" file_offset len);
      full_pwrite outfd buf ~file_offset 0 len >>= fun () ->
      Logs.debug (fun l -> l "write done %d %d" file_offset len);
      Lwt.return_unit
    )
  in
  let rec fn file_offset =
    match insize - file_offset with
    | 0 -> ()
    | remaining ->
       let len = min block_size remaining in
       let t = read_then_write_chunk ~file_offset len in
       reqs := t :: !reqs;
       fn (file_offset + len)
   in
  fn 0;
  Logs.debug (fun l -> l "Waiting on %d reqs" (List.length !reqs));
  Lwt.join !reqs >>= fun () ->
  Logs.debug (fun l -> l "Done all reqs");
  Lwt.return_unit

let run_cp block_size queue_depth infile outfile () =
  Lwt_main.run (
    let open Lwt_unix in
    openfile infile [O_RDONLY] 0 >>= fun infd ->
    openfile outfile [O_WRONLY; O_CREAT; O_TRUNC] 0o644 >>= fun outfd ->
    (fstat infd >|= fun {st_size; _} -> st_size) >>= fun insize ->
    let pool = Lwt_pool.create queue_depth (fun () -> Lwt.return @@ Bytes.make block_size '\000') in
    Logs.debug (fun l -> l "lwtcp: %s -> %s size %d queue %d bs %d" infile outfile insize queue_depth block_size);
    copy_file infd outfd insize block_size pool >>= fun () ->
    Logs.debug (fun l -> l "lwtcp: done");
    Lwt_unix.close outfd >>= fun () ->
    Lwt_unix.close infd)
