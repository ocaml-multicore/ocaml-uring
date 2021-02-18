type ioreq =
| Nothing
| Read of Baregion.chunk
| Write of Baregion.chunk

let pp_ioreq ppf r =
  Fmt.pf ppf "%s" 
    (match r with
     | Nothing -> "nothing"
     | Read c -> Fmt.strf "read/%d" (Baregion.to_offset c)
     | Write c -> Fmt.strf "write/%d" (Baregion.to_offset c)
    )

type runnable =
| Thread : ('a, unit) continuation * 'a -> runnable

type t = {
  uring: ioreq Uring.t;
  mem: Baregion.t;
  run_q : runnable Queue.t;
  io_q : (ioreq, (unit, unit) continuation) Hashtbl.t;
}

effect Fork  : (t -> unit) -> unit
effect Yield : unit
effect Queued : ioreq -> unit

let rec schedule st =
  Logs.debug (fun l -> l "entering scheduler, blocking on Uring");
  (* FIXME when do we submit? idle thread? *)
  let _ = Uring.submit st.uring in
  (* FIXME if res < 1 then deadlock *)
  (* FIXME assumes this is just for IO for now *)
  match Uring.wait st.uring with
  | None -> 
     Logs.debug (fun l -> l "wait returned none");
    schedule st (* TODO this is a bad situation to be in, likely fatal *)
  | Some (req, _res) ->
     Logs.debug (fun l -> l "wait returned");
     let k = Hashtbl.find st.io_q req in
     Hashtbl.remove st.io_q req;
     continue k ()

let enqueue_thread st k x =
  Queue.push (Thread (k, x)) st.run_q

let enqueue_uring_req st k req =
  Logs.debug (fun l -> l "queuing io req %a" pp_ioreq req);
  Hashtbl.add st.io_q req k

let fork f =
  perform (Fork f)

let yield () =
  perform Yield

let read {uring; mem; _} ?file_offset fd len =
  let chunk = Baregion.alloc mem in
  let req = Read chunk in
  Uring.read uring ?file_offset fd (Baregion.to_offset chunk) len req;
  Logs.debug (fun l -> l "read: performing Queued effect");
  perform (Queued req);
  Logs.debug (fun l -> l "read: woken up after read");
  Baregion.to_bigstring mem chunk

let write {uring; mem; _} ?file_offset fd len =
  let chunk = Baregion.alloc mem in
  let req = Write chunk in
  Uring.write uring ?file_offset fd (Baregion.to_offset chunk) len req;
  perform (Queued req)

let run main =
  Logs.debug (fun l -> l "starting run");
  (* TODO unify this allocation API around baregion/uring *)
  let blocksize = 4096 in
  let slots = 1024 in
  let fixed_buf_len = slots * blocksize in
  let queue_depth = 64 in
  let uring = Uring.create ~fixed_buf_len ~queue_depth ~default:Nothing () in
  let buf = Uring.buf uring in 
  let mem = Baregion.init ~blocksize buf slots in
  let run_q = Queue.create () in
  let io_q = Hashtbl.create queue_depth in
  let st = { mem; uring; run_q; io_q } in
  Logs.debug (fun l -> l "starting main thread");
  let rec fork st fn =
    match fn st with
    | () ->
      Logs.debug (fun l -> l "enter scheduler");
       schedule st
    | exception exn ->
       Logs.err (fun l -> l "exn: %a" Fmt.exn exn);
       schedule st
    | effect (Queued req) k ->
      enqueue_uring_req st k req;
    | effect Yield k ->
       enqueue_thread st k ();
       schedule st
    | effect (Fork f) k ->
       enqueue_thread st k ();
       fork st f
   in
   fork st main;
   schedule st;
   Logs.debug (fun l -> l "exit")
