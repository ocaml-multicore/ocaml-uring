module C = Configurator.V1

let () =
  C.main ~name:"discover" (fun c ->
      C.C_define.import c ~c_flags:["-D_GNU_SOURCE"] ~includes:["fcntl.h"; "poll.h"; "sys/uio.h"] C.C_define.Type.[
          "POLLIN", Int;
          "POLLOUT", Int;
          "POLLERR", Int;
          "POLLHUP", Int;

          "O_RDONLY", Int;
          "O_WRONLY", Int;
          "O_RDWR", Int;
          "O_CREAT", Int;
          "O_EXCL", Int;
          "O_NOCTTY", Int;
          "O_TRUNC", Int;
          "O_APPEND", Int;
          "O_NONBLOCK", Int;
          "O_DSYNC", Int;
          "O_DIRECT", Int;
          "O_LARGEFILE", Int;
          "O_DIRECTORY", Int;
          "O_NOFOLLOW", Int;
          "O_NOATIME", Int;
          "O_CLOEXEC", Int;
          "O_SYNC", Int;
          "O_PATH", Int;
          "O_TMPFILE", Int;

          "AT_FDCWD", Int;

          "sizeof(struct iovec)", Int;
        ]
      |> List.map (function
          | name, C.C_define.Value.Int v ->
            let name = if name = "sizeof(struct iovec)" then "sizeof_iovec" else name in
            Printf.sprintf "let %s = 0x%x" (String.lowercase_ascii name) v
          | _ -> assert false
        )
      |> C.Flags.write_lines "config.ml"
    )
