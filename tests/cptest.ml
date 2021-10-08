open Bos
open Rresult
open R.Infix

let _reset_buffer_cache () =
   let shc = Cmd.(v "sh" % "-c" % "echo 1 > /proc/sys/vm/drop_caches") in
   OS.Cmd.run shc

let fill_file_with_random ~count dst =
   let ofile = Fmt.str "of=%s" dst in
   let count = Fmt.str "count=%d" count in
   Cmd.(v "dd" % "if=/dev/urandom" % ofile % "bs=27k" % count) |>
   OS.Cmd.run

let block_size = 32 * 1024
let queue_depth = 64
let count = 50000

let run_cp_test impl ~block_size ~queue_depth count =
  let fname_in = Fmt.str "cptest-%d.in" count in
  let fname_out = Fmt.str "cptest-%d.out" count in
  (if Sys.file_exists fname_in then Ok () else
  fill_file_with_random ~count fname_in) >>= fun () ->
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
  List.iter
    (fun v -> Bechamel_notty.Unit.add v (Measure.unit v))
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix
    
let () =
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 } in
  let results, _ = benchmark () in
  img (window, results) |> eol |> output_image
