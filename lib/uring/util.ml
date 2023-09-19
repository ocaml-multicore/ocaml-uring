module Bstruct = struct
  type t = {
    buffer : bytes;
    off : int;
    len : int;
  }

  let empty = { buffer = Bytes.empty; off = 0; len = 0 }

  let length t = t.len

  let copy_to_string src srcoff len =
    if len < 0 || srcoff < 0 || src.len - srcoff < len then
      failwith "Error copying to string!"
    else
      let b = Bytes.create len in
      Bytes.blit src.buffer (src.off+srcoff) b 0 len;
      (* The following call is safe, since b is not visible elsewhere. *)
      Bytes.unsafe_to_string b

  let blit_from_string src srcoff dst dstoff len =
    if len < 0 || srcoff < 0 || String.length src - srcoff < len then
      invalid_arg "Bstruct blit"
    else if dstoff < 0 || dst.len - dstoff < len then
      invalid_arg "Bstruct blit"
    else
      String.blit src srcoff dst.buffer
        (dst.off+dstoff) len

  let to_string ?(off=0) ?len t =
    let len = match len with None -> length t - off | Some l -> l in
    copy_to_string t off len

  let pp_t ppf t =
    Format.fprintf ppf "[%d,%d](%d)" t.off t.len (Bytes.length t.buffer)
  let string_t ppf str =
    Format.fprintf ppf "[%d]" (String.length str)
  let bytes_t ppf str =
    Format.fprintf ppf "[%d]" (Bytes.length str)

  let err fmt =
    let b = Buffer.create 20 in                         (* for thread safety. *)
    let ppf = Format.formatter_of_buffer b in
    let k ppf = Format.pp_print_flush ppf (); invalid_arg (Buffer.contents b) in
    Format.kfprintf k ppf fmt

  let check_bounds t len =
    len >= 0 && Bytes.length t.buffer >= len

  let err_shift t = err "Bstruct.shift %a %d" pp_t t
  let err_shiftv n = err "Bstruct.shiftv short by %d" n

  let shift t amount =
    let off = t.off + amount in
    let len = t.len - amount in
    if amount < 0 || amount > t.len || not (check_bounds t (off+len)) then
      err_shift t amount
    else { t with off; len }

  let rec skip_empty = function
    | t :: ts when t.len = 0 -> skip_empty ts
    | x -> x

  let rec shiftv ts = function
    | 0 -> skip_empty ts
    | n ->
      match ts with
      | [] -> err_shiftv n
      | t :: ts when n >= t.len -> shiftv ts (n - t.len)
      | t :: ts -> shift t n :: ts
end

module Slab = struct
  type t = {
    size : int;
    mutable free_ptr : int;
    mutable bytes : bytes;
    mutable slices : (int * int) list;
  }

  let create size = { size; free_ptr = 0; bytes = Bytes.create size; slices = [] }

  (* It would be nice if the bstruct here was just the raw pointer to the byte
     array instead of probably all the extra header bytes... *)
  let slice t len =
    if t.free_ptr + len > t.size then invalid_arg "No space"
    else begin
      let b = { 
        Bstruct.buffer = t.bytes;
        off = t.free_ptr;
        len = len;
      } in
      t.slices <- (t.free_ptr, len) :: t.slices;
      t.free_ptr <- t.free_ptr + len;
      b
    end

  let slice_string t s =
    let len = String.length s in
    let b = slice t len in
    Bstruct.blit_from_string s 0 b 0 len;
    b

  let slice_strings t s =
    List.map (slice_string t) s
end