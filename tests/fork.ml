external set_signal : unit -> unit = "test_set_signal"

let main () =
  set_signal ();
  for _ = 1 to 10000 do
    match Unix.fork () with
    | 0 -> Unix._exit 1
    | _child -> ()
  done

let () =
  let uring = Uring.create ~queue_depth:64 () in
  Effect.Deep.match_with main ()
    { retc = ignore;
      exnc = raise;
      effc = (fun _ -> None);
    };
  Uring.exit uring
