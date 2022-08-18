(* This benchmark uses [Uring.readv] to read from /dev/zero in a loop.
   This is intended to stress the sketch buffer. *)

let buffer_size = 100   (* Use a small buffer to stress the system more *)
let n_concurrent = 16   (* How many requests to have active at once *)
let n_iters = 1_000_000 (* How many times to accept and resubmit *)

let rec wait t handle =
  match Uring.peek t with
  | Some { result; data = buf } -> handle result buf
  | None ->
    match Uring.wait t with
    | None -> wait t handle
    | Some { result; data = buf } -> handle result buf

let run_bechmark ~polling_timeout fd =
  let got = ref 0 in
  (* For polling mode, [queue_depth] needs to be slightly larger than [n_concurrent] or submission
     occasionally fails for some reason. *)
  let t = Uring.create ?polling_timeout ~queue_depth:(n_concurrent * 2) () in
  (* We start by submitting [n_concurrent] reads. *)
  for _ = 1 to n_concurrent do
    let buf = Cstruct.create buffer_size in
    let _job : _ Uring.job = Uring.readv t fd [buf] ~file_offset:Optint.Int63.zero [buf] |> Option.get in
    ()
  done;
  assert (Uring.submit t = n_concurrent);
  (* Then we wait for reads to complete. Each time a read returns, we immediately submit another. *)
  let t0 = Unix.gettimeofday () in
  for _ = 1 to n_iters do
    wait t (fun result bufs ->
        if result < 0 then (
          raise (Unix.Unix_error (Uring.error_of_errno result, "readv", ""))
        ) else (
          got := !got + result;
          let _job : _ Uring.job = Uring.readv t fd bufs ~file_offset:Optint.Int63.zero bufs |> Option.get in
          ()
        )
      )
  done;
  (* Get a snapshot of the stats before letting things finish. *)
  let stats = Uring.get_debug_stats t in
  let t1 = Unix.gettimeofday () in
  let time = t1 -. t0 in
  let got = float !got /. (1024. *. 1024.)  in
  let polling = polling_timeout <> None in
  Fmt.pr "@[<v2>Read %.2f MB in %.2f seconds (%.2f MB/second) # buffer_size=%d, polling=%b@,%a@]@."
    got time (got /. time) buffer_size polling
    Uring.Stats.pp stats;
  (* Finally, drain the remaining reads and shut down the ring. *)
  for _ = 1 to n_concurrent do
    wait t (fun _result _buf -> ())
  done;
  Uring.exit t

let () =
  let fd = Unix.openfile "/dev/zero" Unix.[O_RDONLY] 0 in
  run_bechmark fd ~polling_timeout:None;
  run_bechmark fd ~polling_timeout:(Some 1000);
  run_bechmark fd ~polling_timeout:None;
  run_bechmark fd ~polling_timeout:(Some 1000);
  Unix.close fd
