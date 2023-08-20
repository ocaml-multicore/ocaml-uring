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
      let at_flags, mask_flags, attr_flags =
        let new_flags = 
          C.C_define.Type.[
            (* Masks *)
            "STATX_MNT_ID", Int;
            "STATX_DIOALIGN", Int;
            (* File Attributes *)
            "STATX_ATTR_VERITY", Int;
            "STATX_ATTR_DAX", Int;
          ]
        in
        let new_flag_prelude =
          let def_flag = function
            | name, C.C_define.Type.Int -> 
              Printf.sprintf "#ifndef %s\n#define %s 0\n#endif\n" name name;
            | _ -> assert false
          in
          String.concat "" (List.map def_flag new_flags)
        in
        C.C_define.import c ~c_flags:["-D_GNU_SOURCE"; "-I"; Filename.concat (Sys.getcwd ()) "include"]
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

            "STATX_ATTR_COMPRESSED", Int;
            "STATX_ATTR_IMMUTABLE", Int;
            "STATX_ATTR_APPEND", Int;
            "STATX_ATTR_NODUMP", Int ;
            "STATX_ATTR_ENCRYPTED", Int;
          ] @ new_flags)
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
      let op_sig = List.map (fun (name, _) -> Printf.sprintf "  val %s : t" name) ops in
      let op_struct = List.map (fun (name, v) -> Printf.sprintf "  let %s = 0x%x" name v) ops in
      let at_struct = List.map (fun (name, v) -> Printf.sprintf "  let %s = 0x%x" name v) at_flags in
      let mask_struct = List.map (fun (name, v) -> Printf.sprintf "    let %s = 0x%x" name v) mask_flags in
      let attr_struct = List.map (fun (name, v) -> Printf.sprintf "    let %s = 0x%x" name v) attr_flags in
      C.Flags.write_lines "uring_config.ml"
        (defs @
         ["module Op : sig";
          "  type t";
         ] @ op_sig @ [
           "end = struct";
           "  type t = int"
         ] @ op_struct @ [
           "end"
         ] @ [
          "module Statx = struct";
          "  module Flags = struct";
         ] @ at_struct @ [
          "  end";
          "  module Mask = struct";
         ] @ mask_struct @ [
          "  end";
          "  module Attr = struct";
         ] @ attr_struct @ [
          "  end";
          "end"
         ])
    )
