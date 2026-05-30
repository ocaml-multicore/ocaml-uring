(* Carve up a region of contiguous memory for use
 * by the uring IO stack *)

(* TODO turn into a variable length slab allocator *)
type t = {
  buf: Cstruct.buffer;
  block_size: int;
  freelist: int Queue.t;
}

type chunk = t * int

exception No_space

let init ~block_size buf slots =
  if block_size < 0 then
    invalid_arg (Printf.sprintf "Region.init: negative block_size %d" block_size);
  if slots < 0 then
    invalid_arg (Printf.sprintf "Region.init: negative slots %d" slots);
  let needed = block_size * slots in
  let available = Bigarray.Array1.dim buf in
  if needed > available then
    invalid_arg
      (Printf.sprintf "Region.init: block_size %d * slots %d = %d exceeds buffer size %d"
         block_size slots needed available);
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

let to_cstruct ?len (t, chunk) =
  Cstruct.of_bigarray ~off:chunk ~len:(length_option t len) t.buf

let to_bigstring ?len (t, chunk) =
  Bigarray.Array1.sub t.buf chunk (length_option t len)

let to_string ?len (t, chunk) =
  Cstruct.to_string (to_cstruct ?len (t, chunk))

let avail {freelist;_} = Queue.length freelist

let to_offset (_,t) = t
