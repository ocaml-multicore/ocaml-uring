module C = Configurator.V1

let () =
  C.main ~name:"discover" (fun c ->
      C.C_define.import c ~includes:["poll.h"] C.C_define.Type.[
          "POLLIN", Int;
          "POLLOUT", Int;
          "POLLERR", Int;
          "POLLHUP", Int;
        ]
      |> List.map (function
          | name, C.C_define.Value.Int v ->
            Printf.sprintf "let %s = 0x%x" (String.lowercase_ascii name) v
          | _ -> assert false
        )
      |> C.Flags.write_lines "config.ml"
    )
