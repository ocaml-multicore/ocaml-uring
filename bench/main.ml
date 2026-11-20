open Bechamel

let noop_run queue_depth =
  Staged.stage (fun t ->
      for i = 1 to queue_depth do
        let r = Uring.noop t i in
        assert (r <> None)
      done;
      let submitted = Uring.submit t in
      assert (submitted = queue_depth);
      for _ = 1 to queue_depth do
        ignore (Uring.wait t : _ Uring.completion_option)
      done)

module Unix_sockets = struct
  type t = {
    uring : [`Send | `Recv of Uring.Msghdr.t] Uring.t;
    a : Unix.file_descr * Unix.sockaddr;
    b : Unix.file_descr * Unix.sockaddr;
  }

  let sock (fd, _) = fd
  let addr (_, addr) = addr

  let run =
    let send_buf = [Cstruct.of_string "Hello"] in
    let recv_buf = [Cstruct.of_string "XXXXX"] in
    Staged.stage (fun t ->
        let rec await = function
          | 0 -> ()
          | n ->
            match Uring.wait t.uring with
            | None -> await n
            | Some { result; data } ->
              begin
                match data with
                | `Send -> assert (Uring.Res.int_exn result "sendmsg" "" = 5)
                | `Recv hdr ->
                  assert (Uring.Res.int_exn result "recvmsg" "" = 5);
                  assert (Uring.Msghdr.get_msg_name hdr = (addr t.a))
              end;
              await (n - 1)
        in
        let hdr = Uring.Msghdr.create_recv ~msg_name:true recv_buf in
        for _ = 1 to 1000 do
          let sent = Uring.send_msg t.uring (sock t.a) ~dst:(addr t.b) send_buf `Send in
          assert (sent <> None);
          let recv = Uring.recv_msg t.uring (sock t.b) hdr (`Recv hdr) in
          assert (recv <> None);
          let submitted = Uring.submit t.uring in
          assert (submitted = 2);
          await submitted
        done
      )

  (* Although we ask for a [Test.uniq] resource, Bechamel does seem to allocate
     two at once, so make the names unique. *)
  let next =
    let i = ref 0 in
    fun () -> incr i; !i

  let make_abstract x =
    let i = next () in
    let addr = Unix.ADDR_UNIX (Printf.sprintf "\000ocaml-uring-test-%d-%s.sock" i x) in
    let sock = Unix.(socket PF_UNIX SOCK_DGRAM 0) in
    Unix.bind sock addr;
    (sock, addr)

  let allocate () =
    let uring = Uring.create ~queue_depth:16 () in
    let a = make_abstract "a" in
    let b = make_abstract "b" in
    { uring; a; b }

  let free { uring; a; b } =
    Unix.close (sock a);
    Unix.close (sock b);
    Uring.exit uring

  let test =
    Test.make_with_resource Test.uniq run ~allocate ~free
      ~name:"unix_sockets" 
end

let suite =
  let noop =
    Test.make_indexed_with_resource ~name:"noop" Test.uniq ~fmt:"%s %7d"
      ~args:[ 10; 30; 100; 300; 1_000; 3_000; 10000 ]
      ~allocate:(fun queue_depth -> Uring.create ~queue_depth ())
      ~free:Uring.exit
      noop_run
  in
  Test.make_grouped ~name:"uring" @@ [
    noop;
    Unix_sockets.test;
  ]

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
