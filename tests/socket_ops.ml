open Printf

let () =
  let queue_depth = 128 in
  let t = Uring.create ~queue_depth () in

  (* Create server socket - Unix.socket is necessary as io_uring doesn't have socket creation *)
  let server_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.set_nonblock server_sock;
  printf "Server socket created\n";

  (* Create an address to bind to *)
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 0) in

  (* Use io_uring for bind operation *)
  let () =
    match Uring.bind t server_sock addr () with
    | None -> failwith "Failed to submit bind operation"
    | Some _job ->
        let _submitted = Uring.submit t in
        match Uring.wait t with
        | Uring.None -> failwith "No completion for bind"
        | Uring.Unit _ ->
            print_endline "Bind completed successfully:"
        | Uring.Error { result; data = _ } ->
            Uring.close t server_sock () |> ignore;
            Uring.submit t |> ignore;
            Uring.exit t;
            failwith (sprintf "Bind failed: %s" (Unix.error_message result))
        | Uring.Int _ | Uring.FD _ ->
            failwith "Unexpected return from bind operation"
  in

  (* Use io_uring for listen operation *)
  let backlog = 10 in
  let () =
    match Uring.listen t server_sock backlog () with
    | None -> failwith "Failed to submit listen operation"
    | Some _job ->
        let _submitted = Uring.submit t in
        match Uring.wait t with
        | Uring.None -> failwith "No completion for listen"
        | Uring.Unit _ ->
            print_endline "Listen completed successfully"
        | Uring.Error { result; data = _ } ->
            Uring.close t server_sock () |> ignore;
            Uring.submit t |> ignore;
            Uring.exit t;
            failwith (sprintf "Listen failed: %s" (Unix.error_message result))
        | Uring.Int _ | Uring.FD _ ->
            failwith "Unexpected return from listen operation"
  in

  (* Get the actual bound port - Unix.getsockname is necessary for socket introspection *)
  let actual_addr = Unix.getsockname server_sock in
  let port = match actual_addr with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> failwith "Unexpected address type"
  in
  printf "Socket bound and listening on port: %d\n" port;

  (* Test connecting to the bound socket *)
  printf "Testing connection to the bound socket...\n";

  (* Create client socket - Unix.socket is necessary as io_uring doesn't have socket creation *)
  let client_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.set_nonblock client_sock;
  printf "Client socket created\n";

  (* Use io_uring for connect operation *)
  let connect_addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let () =
    match Uring.connect t client_sock connect_addr () with
    | None -> failwith "Failed to submit connect operation"
    | Some _job ->
        let _submitted = Uring.submit t in
        match Uring.wait t with
        | Uring.None -> failwith "No completion for connect"
        | Uring.Unit _ ->
            print_endline "Connect initiated successfully\n"
        | Uring.Error { result = Unix.EINPROGRESS; data = _ } ->
            (* Connect may return -EINPROGRESS for non-blocking sockets, which is normal *)
            print_endline "Connect initiated successfully (result: EINPROGRESS)"
        | Uring.Error { result; data = _ } ->
            Uring.close t client_sock () |> ignore;
            Uring.close t server_sock () |> ignore;
            Uring.submit t |> ignore;
            Uring.exit t;
            failwith (sprintf "Connect failed: %s" (Unix.error_message result))
        | Uring.Int _ | Uring.FD _ ->
            failwith "Unexpected return from connect operation"
  in


  (* Get the client socket's local port - Unix.getsockname is necessary for socket introspection *)
  let client_addr = Unix.getsockname client_sock in
  let client_port = match client_addr with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> failwith "Unexpected address type"
  in
  printf "Client socket connected from port: %d to port: %d\n" client_port port;

  (* Clean up using io_uring close operations *)
  begin match Uring.close t client_sock () with
    | None -> failwith "Failed to submit close for client socket"
    | Some _ -> ()
  end;

  begin match Uring.close t server_sock () with
    | None -> failwith "Failed to submit close for server socket"
    | Some _ -> ()
  end;

  let _submitted = Uring.submit t in

  (* Wait for both close operations to complete *)
  let rec wait_closes pending =
    if pending > 0 then
      match Uring.wait t with
      | Uring.None -> failwith "No completion for close"
      | Uring.Unit _ ->
          wait_closes (pending - 1)
      | Uring.Error { result; data = _ } ->
          printf "Close warning: %s\n" (Unix.error_message result);
          wait_closes (pending - 1)
      | Uring.Int _ | Uring.FD _ ->
          failwith "Unexpected return from close operation"
  in
  wait_closes 2;

  Uring.exit t;
  printf "Test completed successfully!\n"
