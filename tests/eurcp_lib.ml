(* cp(1) built with effects. *)

let read_then_write_chunk infd outfd file_offset len =
  let chunk, len = Euring.read ~file_offset infd len in
  assert (len > 0); (* TODO *)
  let _,r = Euring.write ~file_offset outfd chunk len in
  assert (r = len);
  ()

let copy_file infd outfd insize block_size =
  let rec fn file_offset =
    match insize - file_offset with
    | 0 -> ()
    | remaining ->
       let len = min block_size remaining in
       Euring.fork (fun () -> read_then_write_chunk infd outfd file_offset len);
       fn (file_offset + len)
  in
  fn 0
  (* TODO join *)

let run_cp block_size queue_depth infile outfile () =
  let open Unix in
  let infd = openfile infile [O_RDONLY] 0 in
  let outfd = openfile outfile [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
  let insize = fstat infd |> fun {st_size; _} -> st_size in
  Logs.debug (fun l -> l "eurcp: %s -> %s size %d queue %d bs %d"
    infile outfile insize queue_depth block_size);
  Euring.run (fun () -> copy_file infd outfd insize block_size);
  Logs.debug (fun l -> l "eurcp: done");
  close outfd;
  close infd
