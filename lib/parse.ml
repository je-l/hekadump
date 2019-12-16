open Re
open Printf
open Soup

type int_or_int_range = Uniform of int | MinMax of int * int

(** Some houses list floor count as range, e.g. 3-4 *)
type floor_count = int_or_int_range

type apartment_size = Same of float | Varying of float * float

type rent = int_or_int_range

let initial_page = "https://www.hekaoy.fi/fi/asunnot/kohteet"
let root_page = "https://www.hekaoy.fi"

(** Single page, e.g. https://www.hekaoy.fi/fi/asunnot/kohteet?page=3 *)
type pagination_crawl_result =
  { house_links : string list;
    next_page   : string option;
  }

(** Single row in the apartment "asuntojakauma" table *)
type apartment =
  { residence_type : string;
    sizes          : apartment_size;
    count          : int;
    rent           : rent;
  }

(** Single house building, e.g.
https://www.hekaoy.fi/fi/asunnot/kohteet/hilda-flodinin-kuja-2 *)
type parsed_house =
  { build_year      : int option;
    floor_count     : floor_count option;
    identifier      : int;
    district        : string option;
    apartment_table : apartment list;
  }

(* Pervasives.float_of_string only parses floats with dot *)
let float_of_string_finnish_opt (text : string) : float option =
  let comma = Pcre.regexp "," in
  let sub _ = "." in
  let replaced = Pcre.substitute ~rex:comma ~subst:sub text in
  float_of_string_opt replaced

let float_of_string_finnish (text : string) : float =
  match float_of_string_finnish_opt text with
      None -> failwith (sprintf "cannot parse: %s\n" text)
    | Some t -> t

(*
First remove whitespace, then parse the min/max pair:

1 100 - 1 200 --> 1100-1200 --> [| 1100-1200; 1100; 1200|]
*)
let parse_floor_text (text : string) : floor_count option =
  let whitespace = Pcre.regexp "\\s+" in
  let sub _ = "" in
  let cleaned_text = Pcre.substitute ~rex:whitespace ~subst:sub text in
  let regex = Pcre.regexp "(\\d+)(?:–|-)(\\d+)" in
  let groups = try Some (Pcre.extract ~rex:regex cleaned_text)
    with Not_found -> None in

  match groups with
    | Some [| _; left; right|] ->
        Some (MinMax (int_of_string left, int_of_string right))
    | None -> (match int_of_string_opt cleaned_text with
        None -> None
      | Some e -> Some (Uniform e))
    | _ -> None

let parse_apartment_sizes (text : string) : apartment_size option =
  let regex = Pcre.regexp "(\\d+,\\d+) - (\\d+,\\d+)" in
  let groups = try Some (Pcre.extract ~rex:regex text)
    with Not_found -> None in

  match groups with
    | Some [| _; minimum; maximum|] ->
        Some (Varying (float_of_string_finnish minimum,
                        float_of_string_finnish maximum))
    | _ -> (match float_of_string_finnish_opt text with
        None -> failwith (sprintf "text: %s\n" text)
      | Some e -> Some (Same e))

let parse_apartment_count (text : string) : int option =
  match int_of_string_opt text with
    Some num -> Some num
  | None ->
      let re = Pcre.regexp "(\\d+) kpl" in
      let groups = try Some (Pcre.extract ~rex:re text) with
      Not_found -> None in
      match groups with
        Some [|_; count |] -> Some (int_of_string (String.trim count))
        | _ -> None

type table_row_result =
  ApartmentError of string (* Unexpected row *)
| Anomaly of string (* Weird row which should be skipped *)
| Success of apartment

let parse_apartment_table_row (el : element node) : table_row_result =
  let cells = select "td" el in
  match to_list cells with
    [typ; sizes_text; count_text; raw_rent] -> begin
      match leaf_text typ with
        None -> ApartmentError "empty apartment type"
      | Some residence_type ->
          match leaf_text count_text with
            None -> ApartmentError "unexpected empty count"
          | Some count_contents ->
              match parse_apartment_count count_contents with
                None ->
                  Anomaly (sprintf
                    "non int apartment count: '%s'"
                    count_contents)
              | Some count ->
                  match leaf_text sizes_text with
                    None -> ApartmentError "apartment sizes cell is empty"
                  | Some sizes_raw ->
                      match parse_apartment_sizes sizes_raw with
                        None -> ApartmentError "cannot parse apartment"
                      | Some sizes ->
                        match leaf_text raw_rent with
                          None -> ApartmentError "empty rent cell"
                        | Some parsed_rent ->
                            match parse_floor_text parsed_rent with
                              None -> failwith "cannot parse rent"
                            | Some rent ->
                                Success { residence_type;
                                          sizes;
                                          count;
                                          rent
                                }
      end
  | _ -> ApartmentError "unexpected column count"

let parse_apartment_table (html : soup node) : table_row_result list =
  let rows = select ".asuntojakauma-container tbody tr" html in
  List.map parse_apartment_table_row (to_list rows)

let parse_next_page (html : soup node) : string option =
  match select_one ".pager__item--next a" html with
      None -> None
    | Some el -> match attribute "href" el with
        None -> None
      | Some link -> Some (initial_page ^ link)

let parse_page_houses (html : soup node) : string list =
  let link_elements = select ".node--type-kiinteisto h4 a" html in
  let maybe_links = List.map (attribute "href") (to_list link_elements) in
  let unsafe_filter el = match el with
      Some a -> a
    | None -> failwith "link without href! CSS selector is faulty" in
  let links = List.map unsafe_filter maybe_links in
  List.map (fun e -> root_page ^ e) links
