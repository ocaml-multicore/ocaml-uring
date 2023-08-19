(* Carve up a region of contiguous memory for use
 * by the uring IO stack *)

(* TODO turn into a variable length slab allocator *)
type t = {
  buf: bytes;
  block_size: int;
  freelist: int Queue.t;
}

type chunk = t * int

exception No_space

let init ~block_size buf slots =
  let freelist = Queue.create () in
  for i = 0 to slots - 1 do
    Queue.push (i*block_size) freelist
  done;
  { freelist; block_size; buf }

let alloc t =
  match Queue.pop t.freelist with
  | r -> t, r
  | exception Queue.Empty -> raise No_space

let free ({freelist; _}, v) =
  Queue.push v freelist

let length ({block_size;_}, _) = block_size

let length_option t = function
  | None -> t.block_size
  | Some len ->
    if len > t.block_size then
      invalid_arg (Printf.sprintf "to_cstruct: requested length %d > block size %d" len t.block_size)
    else
      len

let to_bytes ?len (t, chunk) =
  Bytes.sub t.buf chunk (length_option t len)

let to_string ?len (t, chunk) = Bytes.to_string (to_bytes ?len (t, chunk))

let avail {freelist;_} = Queue.length freelist

let to_offset (_,t) = t
