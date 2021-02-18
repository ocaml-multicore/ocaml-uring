(* basic tests using effects *)

open Euring

let setup_log level =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  setup_log (Some Logs.Debug);
  (* TODO expose openfile from euring *)
  let fd = Unix.(handle_unix_error (openfile "test.txt" [O_RDONLY]) 0) in
  run (fun t ->
    let buf = read t fd 5 in
    print_endline (Bigstringaf.to_string buf);
    let buf = read t fd 3 in
    print_endline (Bigstringaf.to_string buf);

  );
