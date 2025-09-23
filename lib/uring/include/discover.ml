module C = Configurator.V1

let include_dir = Filename.concat (Sys.getcwd ()) "include"

let toplevel_defs c =
  C.C_define.import c ~c_flags:["-D_GNU_SOURCE"; "-I"; include_dir]
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
        Gen.hex_val (String.lowercase_ascii name, v)
      | _ -> assert false
    )

let uring_ops = [
  "IORING_OP_NOP";
  "IORING_OP_READV";
  "IORING_OP_WRITEV";
  "IORING_OP_FSYNC";
  "IORING_OP_READ_FIXED";
  "IORING_OP_WRITE_FIXED";
  "IORING_OP_POLL_ADD";
  "IORING_OP_POLL_REMOVE";
  "IORING_OP_SYNC_FILE_RANGE";
  "IORING_OP_SENDMSG";
  "IORING_OP_RECVMSG";
  "IORING_OP_TIMEOUT";
  "IORING_OP_TIMEOUT_REMOVE";
  "IORING_OP_ACCEPT";
  "IORING_OP_ASYNC_CANCEL";
  "IORING_OP_LINK_TIMEOUT";
  "IORING_OP_CONNECT";
  "IORING_OP_FALLOCATE";
  "IORING_OP_OPENAT";
  "IORING_OP_CLOSE";
  "IORING_OP_FILES_UPDATE";
  "IORING_OP_STATX";
  "IORING_OP_READ";
  "IORING_OP_WRITE";
  "IORING_OP_FADVISE";
  "IORING_OP_MADVISE";
  "IORING_OP_SEND";
  "IORING_OP_RECV";
  "IORING_OP_OPENAT2";
  "IORING_OP_EPOLL_CTL";
  "IORING_OP_SPLICE";
  "IORING_OP_PROVIDE_BUFFERS";
  "IORING_OP_REMOVE_BUFFERS";
  "IORING_OP_TEE";
  "IORING_OP_SHUTDOWN";
  "IORING_OP_RENAMEAT";
  "IORING_OP_UNLINKAT";
  "IORING_OP_MKDIRAT";
  "IORING_OP_SYMLINKAT";
  "IORING_OP_LINKAT";
  "IORING_OP_MSG_RING";
  "IORING_OP_FSETXATTR";
  "IORING_OP_SETXATTR";
  "IORING_OP_FGETXATTR";
  "IORING_OP_GETXATTR";
  "IORING_OP_SOCKET";
  "IORING_OP_URING_CMD";
  "IORING_OP_READ_MULTISHOT";
  "IORING_OP_WAITID";
  "IORING_OP_FUTEX_WAIT";
  "IORING_OP_FUTEX_WAKE";
  "IORING_OP_FUTEX_WAITV";
  "IORING_OP_FIXED_FD_INSTALL";
  "IORING_OP_FTRUNCATE";
  "IORING_OP_BIND";
  "IORING_OP_LISTEN";
]

let uring_setup_flags = [
  "IORING_SETUP_IOPOLL";
  (* "IORING_SETUP_SQPOLL" *)  (* Enabled by passing a polling timeout instead *)
  (* "IORING_SETUP_SQ_AFF"; *) (* todo: requires setting sq_thread_cpu too *)
  (* "IORING_SETUP_CQSIZE"; *) (* todo: requires setting cq_entries *)
  "IORING_SETUP_CLAMP";
  (* "IORING_SETUP_ATTACH_WQ"; *) (* todo: requires setting wq_fd *)
  "IORING_SETUP_R_DISABLED";
  "IORING_SETUP_SUBMIT_ALL";
  "IORING_SETUP_COOP_TASKRUN";
  "IORING_SETUP_TASKRUN_FLAG";
  "IORING_SETUP_SQE128";
  "IORING_SETUP_CQE32";
  "IORING_SETUP_SINGLE_ISSUER";
  "IORING_SETUP_DEFER_TASKRUN";
  "IORING_SETUP_NO_MMAP";
  "IORING_SETUP_REGISTERED_FD_ONLY";
  "IORING_SETUP_NO_SQARRAY";
]

let uring_defs c =
  Gen.abstract_module "Op" (
    C.C_define.import c (List.map (fun name -> name, C.C_define.Type.Int) uring_ops)
      ~c_flags:["-D_GNU_SOURCE"; "-I"; include_dir]
      ~includes:["liburing.h"]
    |> List.map (function
        | name, C.C_define.Value.Int v ->
          let prefix_len = String.length "IORING_OP_" in
          let ocaml_name = String.sub name prefix_len (String.length name - prefix_len) |> String.lowercase_ascii in
          (ocaml_name, v)
        | _ -> assert false
      )
  ) @
  Gen.hex_module "Ioring_setup" (
    C.C_define.import c (List.map (fun name -> name, C.C_define.Type.Int) uring_setup_flags)
      ~c_flags:["-D_GNU_SOURCE"; "-I"; include_dir]
      ~includes:["liburing.h"]
    |> List.map (function
        | name, C.C_define.Value.Int v ->
          let prefix_len = String.length "IORING_SETUP_" in
          let ocaml_name = String.sub name prefix_len (String.length name - prefix_len) |> String.lowercase_ascii in
          (ocaml_name, v)
        | _ -> assert false
      )
  )

let stat_flags c =
  let at_flags, mask_flags, attr_flags =
    let new_flags = 
      C.C_define.Type.[
        (* Masks *)
        "STATX_MNT_ID", Int, 0x00001000;
        "STATX_DIOALIGN", Int, 0x00002000;
        (* File Attributes *)
        "STATX_ATTR_COMPRESSED", Int, 0x00000004;
        "STATX_ATTR_IMMUTABLE", Int, 0x00000010;
        "STATX_ATTR_APPEND", Int, 0x00000020;
        "STATX_ATTR_NODUMP", Int, 0x00000040;
        "STATX_ATTR_ENCRYPTED", Int, 0x00000800;
        "STATX_ATTR_VERITY", Int, 0x00100000;
        "STATX_ATTR_DAX", Int, 0x00200000;
      ]
    in
    let new_flag_prelude =
      let def_flag = function
        | name, C.C_define.Type.Int, v ->
          Printf.sprintf "#ifndef %s\n#define %s %d\n#endif\n" name name v;
        | _ -> assert false
      in
      String.concat "" (List.map def_flag new_flags)
    in
    C.C_define.import c ~c_flags:["-D_GNU_SOURCE"; "-I"; include_dir]
      ~prelude:({|#include <sys/stat.h>
#ifndef STATX_TYPE
#include <linux/stat.h>
#endif
|} ^ new_flag_prelude)
      ~includes:["fcntl.h" ]
      (C.C_define.Type.[
          "AT_EMPTY_PATH", Int;
          "AT_NO_AUTOMOUNT", Int;
          "AT_SYMLINK_NOFOLLOW", Int;
          "AT_SYMLINK_FOLLOW", Int;
          "AT_STATX_SYNC_AS_STAT", Int;
          "AT_STATX_FORCE_SYNC", Int;
          "AT_STATX_DONT_SYNC", Int;

          "STATX_TYPE", Int;
          "STATX_MODE", Int;
          "STATX_NLINK", Int;
          "STATX_UID", Int;
          "STATX_GID", Int;
          "STATX_ATIME", Int;
          "STATX_MTIME", Int;
          "STATX_CTIME", Int;
          "STATX_INO", Int;
          "STATX_SIZE", Int;
          "STATX_BLOCKS", Int;
          "STATX_BASIC_STATS", Int;
          "STATX_BTIME", Int;
        ] @ List.map (fun (n, t, _) -> n, t) new_flags)
    |> List.fold_left (fun (ats, stats, attrs) (v, k) -> match String.split_on_char '_' v, k with
        | "AT" :: name, C.C_define.Value.Int v ->
          let ocaml_name =  String.lowercase_ascii (String.concat "_" name) in
          ((ocaml_name, v) :: ats, stats, attrs)
        | "STATX" :: "ATTR" :: name, C.C_define.Value.Int v ->
          let name = String.concat "_" name |> String.lowercase_ascii in
          let ocaml_name = match name with
            | "type" -> "type'"
            | v -> v
          in
          (ats, stats, (ocaml_name, v) :: attrs)
        | "STATX" :: name, C.C_define.Value.Int v ->
          let name = String.concat "_" name |> String.lowercase_ascii in
          let ocaml_name = match name with
            | "type" -> "type'"
            | v -> v
          in
          (ats, (ocaml_name, v) :: stats, attrs)
        | _ -> assert false
      ) ([], [], [])
  in
  Gen.hex_module "At" at_flags @
  ["module Statx = struct"] @
  Gen.indent_lines (Gen.hex_module "Mask" mask_flags) @
  Gen.indent_lines (Gen.hex_module "Attr" attr_flags) @
  ["end"]

let () =
  C.main ~name:"discover" (fun c ->
      C.Flags.write_lines "config.ml" @@ List.flatten @@ List.map (fun f -> f c) [
        toplevel_defs;
        uring_defs;
        stat_flags;
      ]
    )
