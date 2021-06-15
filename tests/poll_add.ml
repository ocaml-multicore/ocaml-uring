let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  let t = Uring.create ~queue_depth:1 () in
  let readable, writable = Unix.pipe () in
  let r = Uring.poll_add t readable Uring.Poll_mask.(pollin + pollerr) () in assert(r <> None);
  let res = Uring.submit t in
  Printf.eprintf "submitted %d\n%!" res;
  let sent = Unix.write writable (Bytes.of_string "!") 0 1 in
  assert (sent = 1);
  let rec retry () =
    match Uring.wait t with
    | None -> retry ()
    | Some { result; _ } -> result
  in
  let res = retry () in
  Printf.eprintf "poll_add: %x\n%!" res;
  Uring.exit t
