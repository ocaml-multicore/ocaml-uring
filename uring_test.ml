let ring = Uring.ring_setup 1024

let server_fd = Unix.socket PF_INET SOCK_STREAM 0

let client_fds = ref []

let rec handle_accept client_fd =
    Printf.printf "Got connection!\n";
    Uring.ring_queue_accept ring server_fd handle_accept;
    Uring.ring_submit ring |> ignore;
    client_fds := client_fd :: !client_fds

let rec server_loop () =
    Uring.ring_wait ring;
    server_loop ()

let () =
    Unix.bind server_fd (ADDR_INET (Unix.inet_addr_any, 8080));
    Unix.listen server_fd 64;
    Uring.ring_queue_accept ring server_fd handle_accept;
    Uring.ring_submit ring |> ignore;
    server_loop ()