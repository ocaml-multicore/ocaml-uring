module C = Configurator.V1

let () =
  C.main ~name:"discover" (fun c ->
      let defs =
        C.C_define.import c ~c_flags:["-D_GNU_SOURCE"; "-I"; Filename.concat (Sys.getcwd ()) "include"]
          ~includes:["fcntl.h"; "poll.h"; "sys/uio.h"; "limits.h"; "liburing.h"]
          C.C_define.Type.[
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
            "IOV_MAX", Int;

            "sizeof(struct iovec)", Int;
            "sizeof(struct __kernel_timespec)", Int;
          ]
        |> List.map (function
            | name, C.C_define.Value.Int v ->
              let name = 
                match name with
                | "sizeof(struct iovec)" -> "sizeof_iovec"
                | "sizeof(struct __kernel_timespec)" -> "sizeof_kernel_timespec"
                | nm -> nm 
              in
              Printf.sprintf "let %s = 0x%x" (String.lowercase_ascii name) v
            | _ -> assert false
          )
      in
      let ops =
        C.C_define.import c ~c_flags:["-D_GNU_SOURCE"; "-I"; Filename.concat (Sys.getcwd ()) "include"]
          ~includes:["liburing.h"]
          C.C_define.Type.[
            "IORING_OP_NOP", Int;
            "IORING_OP_READV", Int;
            "IORING_OP_WRITEV", Int;
            "IORING_OP_FSYNC", Int;
            "IORING_OP_READ_FIXED", Int;
            "IORING_OP_WRITE_FIXED", Int;
            "IORING_OP_POLL_ADD", Int;
            "IORING_OP_POLL_REMOVE", Int;
            "IORING_OP_SYNC_FILE_RANGE", Int;
            "IORING_OP_SENDMSG", Int;
            "IORING_OP_RECVMSG", Int;
            "IORING_OP_TIMEOUT", Int;
            "IORING_OP_TIMEOUT_REMOVE", Int;
            "IORING_OP_ACCEPT", Int;
            "IORING_OP_ASYNC_CANCEL", Int;
            "IORING_OP_LINK_TIMEOUT", Int;
            "IORING_OP_CONNECT", Int;
            "IORING_OP_FALLOCATE", Int;
            "IORING_OP_OPENAT", Int;
            "IORING_OP_CLOSE", Int;
            "IORING_OP_FILES_UPDATE", Int;
            "IORING_OP_STATX", Int;
            "IORING_OP_READ", Int;
            "IORING_OP_WRITE", Int;
            "IORING_OP_FADVISE", Int;
            "IORING_OP_MADVISE", Int;
            "IORING_OP_SEND", Int;
            "IORING_OP_RECV", Int;
            "IORING_OP_OPENAT2", Int;
            "IORING_OP_EPOLL_CTL", Int;
            "IORING_OP_SPLICE", Int;
            "IORING_OP_PROVIDE_BUFFERS", Int;
            "IORING_OP_REMOVE_BUFFERS", Int;
            "IORING_OP_TEE", Int;
            "IORING_OP_SHUTDOWN", Int;
            "IORING_OP_RENAMEAT", Int;
            "IORING_OP_UNLINKAT", Int;
            "IORING_OP_MKDIRAT", Int;
            "IORING_OP_SYMLINKAT", Int;
            "IORING_OP_LINKAT", Int;
            "IORING_OP_MSG_RING", Int;
            "IORING_OP_FSETXATTR", Int;
            "IORING_OP_SETXATTR", Int;
            "IORING_OP_FGETXATTR", Int;
            "IORING_OP_GETXATTR", Int;
            "IORING_OP_SOCKET", Int;
            "IORING_OP_URING_CMD", Int;
          ]
        |> List.map (function
            | name, C.C_define.Value.Int v ->
              let prefix_len = String.length "IORING_OP_" in
              let ocaml_name = String.sub name prefix_len (String.length name - prefix_len) |> String.lowercase_ascii in
              (ocaml_name, v)
            | _ -> assert false
          )
      in
      let op_sig = List.map (fun (name, _) -> Printf.sprintf "  val %s : t" name) ops in
      let op_struct = List.map (fun (name, v) -> Printf.sprintf "  let %s = 0x%x" name v) ops in
      C.Flags.write_lines "config.ml"
        (defs @
         ["module Op : sig";
          "  type t";
         ] @ op_sig @ [
           "end = struct";
           "  type t = int"
         ] @ op_struct @ [
           "end"
         ])
    )
