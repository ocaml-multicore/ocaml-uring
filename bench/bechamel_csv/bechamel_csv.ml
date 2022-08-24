open Bechamel

let pad width v =
  let padding = String.make (width - String.length v) ' ' in
  padding ^ v

let pp_row ~width f data =
  data |> List.iteri (fun i v ->
      if i > 0 then Fmt.comma f ();
      Fmt.pf f "%s" (pad width.(i) v)
    )

let pp f results =
  let results = Hashtbl.to_seq results |> Array.of_seq in
  Array.sort (fun (n1, _) (n2, _) -> String.compare n1 n2) results;     (* Sort columns *)
  let rows = snd results.(0) |> Hashtbl.to_seq |> Seq.map fst |> Array.of_seq in
  let width = Array.make (Array.length results + 1) 0 in
  width.(0) <- String.length "name, ";
  results |> Array.iteri (fun i (name, _) -> width.(i + 1) <- String.length name + 2);
  Array.sort String.compare rows;
  let rows =
    rows |> Array.map (fun name ->
        width.(0) <- max width.(0) (String.length name + 2);
        let values =
          results |> Array.mapi (fun i (_, col) ->
              let v =
                match Hashtbl.find col name |> Analyze.OLS.estimates with
                | Some [v] -> Printf.sprintf "%f" v
                | _ -> assert false
              in
              width.(i + 1) <- max width.(i + 1) (String.length v + 2);
              v
            )
        in
        name, values
      )
  in
  let metrics = Array.to_list results |> List.map fst in
  let headings = List.mapi (fun i v -> pad width.(i) v) ("name" :: metrics) in
  Fmt.pf f "@[<v>@[<h>%a@]" Fmt.(list ~sep:comma string) headings;
  rows |> Array.iter (fun (name, data) ->
      Fmt.pf f "@,@[<h>%a@]" (pp_row ~width) (name :: Array.to_list data);
    );
  Fmt.pf f "@]"
