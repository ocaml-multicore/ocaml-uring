let rec await pid =
  match Unix.waitpid [] pid with
  | (_pid, status) -> status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> await pid

let run prog args =
  await (Unix.create_process prog (Array.of_list (prog :: args)) Unix.stdin Unix.stdout Unix.stderr)

let check_status = function
  | Unix.WEXITED 0 -> ()
  | _ -> assert false

let _reset_buffer_cache () =
  Unix.system "echo 1 > /proc/sys/vm/drop_caches" |> check_status

let fill_file_with_random ~count dst =
  run "dd" [ "if=/dev/urandom"; Fmt.str "of=%s" dst; "bs=27k"; Fmt.str "count=%d" count ]
  |> check_status

let block_size = 32 * 1024
let queue_depth = 64
let count = 50000

let run_cp_test impl ~block_size ~queue_depth count =
  let fname_in = Fmt.str "cptest-%d.in" count in
  let fname_out = Fmt.str "cptest-%d.out" count in
  if not (Sys.file_exists fname_in) then fill_file_with_random ~count fname_in;
  impl block_size queue_depth fname_in fname_out ();
  (* TODO check they are the same file *)
  at_exit (fun () -> try Sys.remove fname_in with _ -> ());
  Sys.remove fname_out;
  Ok ()

open Bechamel
open Toolkit

let run_test impl ~block_size ~queue_depth count =
  Staged.stage @@ fun () ->
  run_cp_test impl ~block_size ~queue_depth count

let test_size impl =
  Test.make_indexed ~name:"size" ~fmt:"%s %d" ~args:[ 1000; 10000; 50000 ]
    (run_test impl ~block_size ~queue_depth)

let test_queue_depth impl =
  Test.make_indexed ~name:"queue_depth" ~fmt:"%s %d" ~args:[ 1; 32; 64 ]
    (fun queue_depth -> run_test impl ~block_size ~queue_depth count)

let test_block_size impl =
  Test.make_indexed ~name:"block_size" ~fmt:"%s %d" ~args:[ 1024; (32 * 1024); (128 * 1024) ]
  (fun block_size -> run_test impl ~block_size ~queue_depth count)

let urcp_test =
  Test.make_grouped ~name:"urcp_alloc" (List.map (fun l -> l Urcp_lib.run_cp) [test_size; test_queue_depth; test_block_size])
let urcp_fixed_test =
    Test.make_grouped ~name:"urcp_fixed" (List.map (fun l -> l Urcp_fixed_lib.run_cp) [test_size; test_queue_depth; test_block_size])
let lwt_bytes_test =
  Test.make_grouped ~name:"lwt_bytes" (List.map (fun l -> l Lwtcp_lib.run_cp) [test_size; test_queue_depth; test_block_size])

let test = Test.make_grouped ~name:"cp" [  lwt_bytes_test; urcp_test; urcp_fixed_test ]

let benchmark () =
  let ols = Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |] in
   let instances =
        Instance.[ minor_allocated; major_allocated; monotonic_clock ] in
      let cfg =
        Benchmark.cfg ~limit:2000 ~quota:(Time.second 0.5) ~kde:(Some 1000) () in
      let raw_results = Benchmark.all cfg instances test in
      let results =
        List.map (fun instance -> Analyze.all ols instance raw_results) instances
      in
      let results = Analyze.merge ols instances results in
      (results, raw_results)

let () =
  let results, _ = benchmark () in
  Fmt.pr "%a@." Bechamel_csv.pp results
