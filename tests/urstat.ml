(* stat(1) built with liburing. *)

module S = Uring.Statx

(* TODO move into Uring.Statx? *)
let pp_time f (sec, nsec) =
  let tm = Unix.localtime (Int64.to_float sec) in
  Format.fprintf f "%04d-%02d-%02d %02d:%02d:%02d.%09d +0000"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    nsec

let get_completion_and_print uring =
  let (fname, buf), _ =
    match Uring.wait uring with
    | Some { data; result } -> (data, result)
    | None -> failwith "retry"
  in
  let kind = S.kind buf in
  let amask = S.attributes_mask buf in
  let attrs = S.attributes buf in
  let pp_attr f (a,s) =
    try
      if S.Attr.check ~mask:amask attrs a then
        Format.fprintf f "%s:on " s
      else
        Format.fprintf f "%s:off " s
    with Invalid_argument _ ->
      Format.fprintf f "%s:unsup " s
  in
  let opt_symlink = match kind with
      `Symbolic_link -> Printf.sprintf " -> %s" (Unix.readlink fname) (* TODO no readlink in io_uring? *)
    | _ -> "" in
  Format.printf "  File: %s%s\n  Size: %Lu\t\tBlocks: %Lu\tIO Block: %Lu\t %a\nDevice: %Lu\tInode: %Lu\tLinks: %Lu\nAccess: (%04o/TODO)\tUid: (%Lu/TODO)\tGid: (%Lu/TODO)\nAccess: %a\nModify: %a\nChange: %a\n Birth: %a\nAttrs : %a %a %a %a %a %a %a\n%!"
    fname opt_symlink
    (S.size buf)
    (S.blocks buf)
    (S.blksize buf)
    S.pp_kind (S.kind buf)
    (S.dev buf) (* TODO expose makedev/major/minor *) (S.ino buf) (S.nlink buf)
    (S.perm buf) (S.uid buf) (S.gid buf)
    pp_time (S.atime_sec buf, S.atime_nsec buf)
    pp_time (S.mtime_sec buf, S.mtime_nsec buf)
    pp_time (S.ctime_sec buf, S.ctime_nsec buf)
    pp_time (S.btime_sec buf, S.btime_nsec buf)
    pp_attr (S.Attr.compressed, "compressed")
    pp_attr (S.Attr.immutable, "immutable")
    pp_attr (S.Attr.append, "append")
    pp_attr (S.Attr.nodump, "nodump")
    pp_attr (S.Attr.encrypted, "encrypted")
    pp_attr (S.Attr.verity, "verity")
    pp_attr (S.Attr.dax, "dax")

let submit_stat_request fname buf uring =
  let mask = S.Mask.(basic_stats + btime) in
  let flags = S.Flags.(symlink_nofollow + statx_dont_sync) in
  let _ = Uring.statx uring ~mask fname buf flags (fname,buf) in
  let numreq = Uring.submit uring in
  assert(numreq=1);
  ()

let () =
   let fname = Sys.argv.(1) in
   let buf = S.create () in
   let uring = Uring.create ~queue_depth:1 () in
   submit_stat_request fname buf uring;
   get_completion_and_print uring
