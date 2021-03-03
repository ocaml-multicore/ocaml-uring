(* Carve up a region of contiguous memory for use
 * by the uring IO stack *)

(* TODO turn into a variable length slab allocator *)
type t = {
  buf: Bigstringaf.t;
  block_size: int;
  slots: int;
  freelist: int Queue.t;
}

type chunk = t * int

exception No_space

let init ~block_size buf slots =
  let freelist = Queue.create () in
  for i = 0 to slots - 1 do
    Queue.push (i*block_size) freelist
  done;
  { freelist; slots; block_size; buf }

let alloc t =
  match Queue.pop t.freelist with
  | r -> t, r
  | exception Queue.Empty -> raise No_space

let free ({freelist; _}, v) =
  Queue.push v freelist

let to_bigstring ?len ({buf;block_size;_}, chunk) =
  let len = match len with None -> block_size | Some v -> min v block_size in
  Bigstringaf.sub buf ~off:chunk ~len

let to_string ?len ({buf; block_size;_},chunk) =
  let len = match len with None -> block_size | Some v -> min v block_size in
  Bigstringaf.substring buf ~off:chunk ~len

let avail {freelist;_} = Queue.length freelist

let to_offset (_,t) = t
