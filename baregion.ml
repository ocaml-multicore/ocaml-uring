(* Carve up a region of contiguous memory for use
 * by the uring IO stack *)

(* TODO turn into a variable length slab allocator *)
type t = {
  buf: Bigstringaf.t;
  blocksize: int;
  slots: int;
  freelist: int Queue.t;
}

type chunk = t * int

exception No_space

let init ~blocksize buf slots =
  let freelist = Queue.create () in
  for i = 0 to blocksize - 1 do
    Queue.push (i*blocksize) freelist
  done;
  { freelist; slots; blocksize; buf }

let alloc t =
  match Queue.pop t.freelist with
  | r -> t, r
  | exception Queue.Empty -> raise No_space

let free ({freelist; _}, v) =
  Queue.push v freelist

let to_bigstring ({buf;blocksize;_}, chunk) =
  Bigstringaf.sub buf ~off:chunk ~len:blocksize

let to_string ({buf; blocksize;_},chunk) =
  Bigstringaf.substring buf ~off:chunk ~len:blocksize

let to_offset (_,t) = t
