let () =
  let t = Uring.create ~queue_depth:1 ~default:() () in
  let fd = Unix.(handle_unix_error (openfile "test.txt" [O_RDONLY]) 0) in
  let b1 = Uring.Iovec.alloc_buf 3 in
  let b2 = Uring.Iovec.alloc_buf 7 in
  let iov = Uring.Iovec.alloc [|b1;b2|] in
  Uring.readv t fd iov ();
  let res = Uring.submit t in
  Printf.eprintf "submitted %d\n%!" res;
  let (), res = Uring.wait t in
  Uring.Iovec.free iov;
  Printf.eprintf "res %d\n%!" res;
  Printf.eprintf "%s -- %s\n%!" (Bigstringaf.to_string b1) (Bigstringaf.to_string b2);
  ()

(*
let server_fd = Unix.socket PF_INET SOCK_STREAM 0

let client_fd = ref None

let rec server_loop () =
    match !client_fd with
    | None -> server_loop ()
    | Some(client) ->
    let exit_loop = ref false in
    let read_buf = Bigstringaf.create 4096 in
    Uring.ring_queue_read ring client (fun buf len -> 
        if len = 0 then
                begin
                    Printf.printf "Client disconnected!";
                    exit_loop := true
                end
            else
                begin
                    Printf.printf "Read %d bytes: %s\n%!" len (Bigstringaf.substring buf ~off:0 ~len);
                    Uring.ring_queue_write_full ring client (fun _ _ -> ()) buf len;
                    Uring.ring_submit ring |> ignore;
                end
        ) read_buf 0;
        Uring.ring_submit ring |> ignore;
    Uring.ring_wait ring;
    if !exit_loop then
        ()
    else
        server_loop ()

let () =
    Unix.bind server_fd (ADDR_INET (Unix.inet_addr_any, 8081));
    Unix.listen server_fd 64;
    let (new_client_fd, _) = Unix.accept server_fd in
    Printf.printf "Go connection!\n%!";
    client_fd := Some(new_client_fd);
    server_loop ()
    *)
