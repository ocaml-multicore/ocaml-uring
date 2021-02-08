open Bos
open Rresult
open R.Infix

let _reset_buffer_cache () =
   let shc = Cmd.(v "sh" % "-c" % "echo 1 > /proc/sys/vm/drop_caches") in
   OS.Cmd.run shc

let fill_file_with_random ~count dst =
   let ofile = Fmt.strf "of=%a" Fpath.pp dst in
   let count = Fmt.strf "count=%d" count in
   Cmd.(v "dd" % "if=/dev/random" % ofile % "bs=27k" % count) |>
   OS.Cmd.run

let run_cp_test ~label ~count () =
  let fname_in = Fmt.strf "cptest-%s-%d.in" label count |> Fpath.v in
  let fname_out = Fmt.strf "cptest-%s-%d.out" label count |> Fpath.v in
  let cmd = Cmd.(v Sys.argv.(1) % p fname_in % p fname_out) in
  fill_file_with_random ~count fname_in >>= fun () ->
  OS.Cmd.run cmd >>= fun () ->
  OS.Cmd.run cmd >>= fun () ->
  (* TODO check they are the same file *)
  OS.Path.delete fname_in >>= fun () ->
  OS.Path.delete fname_out

let () =
  run_cp_test ~label:"async-ocaml-uring" ~count:1000 () |>
  function Ok () -> () | Error (`Msg m) -> failwith m
