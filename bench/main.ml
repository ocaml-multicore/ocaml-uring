open Bechamel

let noop_run queue_depth =
  let t = Uring.create ~queue_depth () in
  Staged.stage (fun () ->
      for i = 1 to queue_depth do
        let r = Uring.noop t i in
        assert (r <> None)
      done;
      let submitted = Uring.submit t in
      assert (submitted = queue_depth);
      for _ = 1 to queue_depth do
        ignore (Uring.wait t : _ Uring.completion_option)
      done)

let suite =
  Test.make_indexed ~name:"noop" ~fmt:"%s %7d"
    ~args:[ 10; 30; 100; 300; 1_000; 3_000; 10000 ]
    noop_run

let metrics =
  Toolkit.Instance.[ minor_allocated; major_allocated; monotonic_clock ]

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let quota = Time.second 0.5 in
  let cfg = Benchmark.cfg ~limit:2000 ~quota ~kde:(Some 1000) () in
  let raw_results = Benchmark.all cfg metrics suite in
  List.map (fun i -> Analyze.all ols i raw_results) metrics
  |> Analyze.merge ols metrics

let () =
  let results = benchmark () in
  Fmt.pr "@[<v>%a@]@." Bechamel_csv.pp results
