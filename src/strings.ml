open Printf
open Re.Pcre

let remove_whitespace (text : string) : string =
  let reg = Str.regexp_string " " in
  Str.global_replace reg "" text

let parse_apartment_count (text : string) : int option =
  match int_of_string_opt text with
    Some num -> Some num
  | None ->
      let re = regexp "(\\d+) kpl" in
      let groups = try Some (extract ~rex:re text) with
      Not_found -> None in
      match groups with
        Some [|_; count |] -> Some (int_of_string (String.trim count))
        | _ -> None

(* Pervasives.float_of_string only parses floats with dot *)
let float_of_string_finnish_opt (text : string) : float option =
  let comma = regexp "," in
  let sub _ = "." in
  let replaced = substitute ~rex:comma ~subst:sub text in
  float_of_string_opt replaced

let float_of_string_finnish (text : string) : float =
  match float_of_string_finnish_opt text with
      None -> failwith (sprintf "cannot parse: %s\n" text)
    | Some t -> t
