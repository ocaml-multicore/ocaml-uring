type io_job =
| Noop
| Read : Baregion.chunk * (Baregion.chunk * int, unit) continuation -> io_job
| Write : Baregion.chunk * (Baregion.chunk * int, unit) continuation -> io_job

type runnable =
| Thread : ('a, unit) continuation * 'a -> runnable

type t = {
  uring: io_job Uring.t;
  mem: Baregion.t;
  run_q : runnable Queue.t;
  sleep_q: Zzz.t;
  mutable io_jobs: int;
}

let rec wakeup_paused run_q =
  match Queue.take run_q with
  | Thread (k, v) ->
      continue k v;
      wakeup_paused run_q
  | exception Queue.Empty -> ()

let rec schedule ({run_q; sleep_q; uring; _} as st) =
  (* This is not a fair scheduler *)
  (* Wakeup any paused fibres *)
  wakeup_paused run_q;
  Zzz.restart_threads sleep_q;
  let num_jobs = Uring.submit uring in
  st.io_jobs <- st.io_jobs + num_jobs;
  let timeout = Zzz.select_next sleep_q in
  Logs.debug (fun l -> l "scheduler: %d sub / %d total, timeout %s" num_jobs st.io_jobs
    (match timeout with None -> "inf" | Some v -> string_of_float v));
  if Queue.length run_q = 0 && timeout = None && st.io_jobs = 0 then begin
    Logs.debug (fun l -> l "schedule: exiting");
  end else match Uring.wait ?timeout uring with
  | None -> 
     Logs.debug (fun l -> l "wait returned none");
     schedule st (* TODO this is a bad situation to be in, likely fatal *)
  | Some (runnable, res) -> begin
     st.io_jobs <- st.io_jobs - 1;
     match runnable with
     | Read (chunk, k) -> 
        Logs.debug (fun l -> l "read returned");
        continue k (chunk, res)
     | Write (chunk, k) ->
        Logs.debug (fun l -> l "write returned");
        continue k (chunk, res)
     | Noop -> ()
  end

let enqueue_thread st k x =
  Queue.push (Thread (k, x)) st.run_q

effect Sleep : float -> unit
let sleep d =
  perform (Sleep d)

effect Fork  : (unit -> unit) -> unit
let fork f =
  perform (Fork f)

effect Yield : unit
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
  Uring.write uring ?file_offset fd (Baregion.to_offset chunk) len (Write (chunk, k))
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
  let sleep_q = Zzz.init () in
  let st = { mem; uring; run_q; sleep_q; io_jobs = 0 } in
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
       schedule st
    | effect (EWrite args) k ->
       enqueue_write st k args;
       schedule st
    | effect Yield k ->
       enqueue_thread st k ();
       schedule st
    | effect (Sleep d) k ->
       Zzz.sleep sleep_q d (Some k);
       schedule st
    | effect (Fork f) k ->
       enqueue_thread st k ();
       fork f
   in
   fork main;
   Logs.debug (fun l -> l "exit")
