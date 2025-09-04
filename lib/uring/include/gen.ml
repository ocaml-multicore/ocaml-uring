let ( % ) g f x = g (f x)

let hex_val (n, v) = Printf.sprintf "let %s = 0x%x" n v
let sig_val (n, _) = Printf.sprintf "val %s : t" n

let indent l = "  " ^ l
let indent_lines = List.map indent

let abstract_module name values =
  Printf.sprintf "module %s : sig" name ::
  "  type t" ::
  (List.map (indent % sig_val) values) @
  ["end = struct";
   "  type t = int"
  ] @ List.map (indent % hex_val) values @ [
    "end"
  ]

let hex_module name values =
  Printf.sprintf "module %s = struct" name ::
  List.map (indent % hex_val) values @
  ["end"]
