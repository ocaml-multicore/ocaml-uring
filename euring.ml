type runnable =
| Noop
| Thread : ('a, unit) continuation * 'a -> runnable
| Read : Baregion.chunk * (Baregion.chunk * int, unit) continuation -> runnable
| Write : Baregion.chunk * (Baregion.chunk * int, unit) continuation -> runnable

type t = {
  uring: runnable Uring.t;
  mem: Baregion.t;
  run_q : runnable Queue.t;
}

effect Fork  : (unit -> unit) -> unit
effect Yield : unit

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
  | Some (runnable, res) -> begin
     match runnable with
     | Read (chunk, k) -> 
        Logs.debug (fun l -> l "read returned");
        continue k (chunk, res)
     | Write (chunk, k) ->
        Logs.debug (fun l -> l "write returned");
        continue k (chunk, res)
     | Noop -> ()
     | Thread _ -> assert false
  end

let enqueue_thread st k x =
  Queue.push (Thread (k, x)) st.run_q

let fork f =
  perform (Fork f)

let yield () =
  perform Yield

effect ERead : (int option * Unix.file_descr * int) -> (Baregion.chunk * int)

let enqueue_read {uring; mem; _} k (file_offset,fd,len) =
  let chunk = Baregion.alloc mem in
  Logs.debug (fun l -> l "read: submitting call");
  Uring.read uring ?file_offset fd (Baregion.to_offset chunk) len (Read (chunk, k))
  (* TODO hint to a uring submit thread *)
  
let read ?file_offset fd len =
  let chunk, res = perform (ERead (file_offset, fd, len)) in
  Logs.debug (fun l -> l "read: woken up after read");
  if res < 0 then
    raise (Failure (Fmt.strf "read %d" res)) (* FIXME Unix_error *)
  else
    chunk, res

effect EWrite : (int option * Unix.file_descr * int) -> (Baregion.chunk * int)

let enqueue_write {uring; mem; _} k (file_offset,fd,len) =
  let chunk = Baregion.alloc mem in
  Logs.debug (fun l -> l "write: submitting call");
  Uring.read uring ?file_offset fd (Baregion.to_offset chunk) len (Write (chunk, k))
  (* TODO hint to a uring submit thread *)
 
let write ?file_offset fd len =
  let chunk, res = perform (EWrite (file_offset, fd, len)) in
  Logs.debug (fun l -> l "write: woken up after read");
  if res < 0 then
    raise (Failure (Fmt.strf "write %d" res)) (* FIXME Unix_error *)
  else
    chunk, res

let run main =
  Logs.debug (fun l -> l "starting run");
  (* TODO unify this allocation API around baregion/uring *)
  let blocksize = 4096 in
  let slots = 1024 in
  let fixed_buf_len = slots * blocksize in
  let queue_depth = 64 in
  let uring = Uring.create ~fixed_buf_len ~queue_depth ~default:Noop () in
  let buf = Uring.buf uring in 
  let mem = Baregion.init ~blocksize buf slots in
  let run_q = Queue.create () in
  let st = { mem; uring; run_q } in
  Logs.debug (fun l -> l "starting main thread");
  let rec fork fn =
    match fn () with
    | () ->
       Logs.debug (fun l -> l "enter scheduler");
       schedule st
    | exception exn ->
       Logs.err (fun l -> l "exn: %a" Fmt.exn exn);
       schedule st
    | effect (ERead args) k ->
       enqueue_read st k args;
    | effect (EWrite args) k ->
       enqueue_write st k args;
    | effect Yield k ->
       enqueue_thread st k ();
       schedule st
    | effect (Fork f) k ->
       enqueue_thread st k ();
       fork f
   in
   fork main;
   schedule st;
   Logs.debug (fun l -> l "exit")
