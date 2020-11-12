let ring = Uring.ring_setup 1024

let server_fd = Unix.socket PF_INET SOCK_STREAM 0

let client_fd = ref None

let rec server_loop () =
    match !client_fd with
    | None -> server_loop ()
    | Some(client) ->
    begin
    let read_buf = Bigstringaf.create 4096 in
        Uring.ring_queue_read ring client (fun buf len -> 
            Printf.printf "Read %d bytes: %s\n%!" len (Bigstringaf.substring buf ~off:0 ~len)
        ) read_buf 0;
        Uring.ring_submit ring |> ignore;
    Uring.ring_wait ring;
    server_loop ()
    end

let () =
    Unix.bind server_fd (ADDR_INET (Unix.inet_addr_any, 8081));
    Unix.listen server_fd 64;
    let (new_client_fd, _) = Unix.accept server_fd in
    Printf.printf "Go connection!\n%!";
    client_fd := Some(new_client_fd);
    server_loop ()