open Cohttp_lwt_unix
open Soup
open Printf
open Re

let (>>=), (>|=), return = Lwt.((>>=), (>|=), return)

let initial_page = "https://www.hekaoy.fi/fi/asunnot/kohteet"
let root_page = "https://www.hekaoy.fi"

let debug = false

(** Single page, e.g. https://www.hekaoy.fi/fi/asunnot/kohteet?page=3 *)
type pagination_crawl_result =
  { house_links : string list;
    next_page   : string option;
  }

type int_or_int_range = Uniform of int | MinMax of int * int


(** Some houses list floor count as range, e.g. 3-4 *)
type floor_count = int_or_int_range

type apartment_size = Same of float | Varying of float * float

type rent = int_or_int_range

(** Single row in the apartment "asuntojakauma" table *)
type apartment =
  { residence_type : string;
    sizes          : apartment_size;
    count          : int;
    rent           : rent;
  }

(** Single house building, e.g. https://www.hekaoy.fi/fi/asunnot/kohteet/hilda-flodinin-kuja-2 *)
type parsed_house =
  { build_year      : int option;
    floor_count     : floor_count option;
    identifier      : int;
    district        : string option;
    apartment_table : apartment list;
  }

let cat_maybes (options : 'a option list) : 'a list =
  let is_some a = match a with Some _ -> true | None -> false in
  let unwrap_option a = match a with Some t -> t | None -> failwith "bruh" in
  let not_none = List.filter is_some options in
  List.map unwrap_option not_none

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

type table_row_result =
  ApartmentError of string (* Unexpected row *)
| Anomaly of string (* Weird row which should be skipped *)
| Success of apartment

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
                  Anomaly (sprintf "non int apartment count: '%s'" count_contents)
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

let http_get (url : string) : string Lwt.t =
  Client.get (Uri.of_string url) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body -> body

let fetch_links (url : string) : pagination_crawl_result Lwt.t =
  http_get url >|= fun body ->
  let html = parse body in
  { house_links = parse_page_houses html;
    next_page = parse_next_page html
  }

(** Extract house links from all pages, until no more pages are left. *)
let rec crawl_all_pages (url : string) : string list Lwt.t =
  Lwt_io.printlf "crawling page %s" url >>= fun () ->
  fetch_links url >>= fun result ->
  let { house_links; next_page } = result in

  match next_page with
      None -> return house_links
    | Some next ->
        if debug then return house_links
        else crawl_all_pages next >|= fun next -> house_links @ next

let find_string (page : string) (html : soup node) (selector : string) : string =
  match select_one selector html with
      None -> failwith (sprintf "can't find element from DOM with %s from page %s" selector page)
    | Some e -> match leaf_text e with
        None -> failwith (sprintf "cannot find string text from %s" selector)
      | Some t -> t

let find_integer_opt page html selector =
  let text = find_string page html selector in
  int_of_string_opt text

let find_integer page html selector =
  let text = find_integer_opt page html selector in
  match text with
      None -> failwith (sprintf "cannot parse int from selector %s" selector)
    | Some t -> t


(* There's like one housing with build year 1950-1951. Lets ignore that... *)
let find_year url html =
  let year = find_string url html ".field--name-field-year-built .field__item" in
  let re = Pcre.regexp "(\\d{4} ?- ?\\d{4}|\\d{4}/\\d{2}|\\d{4} ?, ?\\d{4}|\\d{4} ja \\d{4})" in
  match int_of_string_opt year with
    None -> if Pcre.pmatch ~rex:re year then None else failwith (sprintf "unexpected build year %s" year)
  | Some y -> Some y

(** Floor count CSS classes are misleading: either one of these two is used.
The text content might look something like "4 - 6" *)
let parse_floor_count (html : soup node) : floor_count option =
  let element = match select_one ".field--name-field-num-floors-min .field__item" html with
    None -> (match select_one ".field--name-field-num-floors-max .field__item" html with
        None -> None
      | Some e -> Some e)
    | Some t -> Some t in

  match element with
      None -> None
    | Some e -> match leaf_text e with
        None -> failwith "couldnt find floor count text"
      | Some t -> parse_floor_text t

let detect_page_under_construction (html : soup node) : bool =
  let gallery_container = select_one "div.field--name-field-image" html in
  match gallery_container with
    None -> true
    | Some _ -> false

let fetch_house (url : string) : parsed_house option Lwt.t =
  http_get url >>= fun body ->
  let html = parse body in

  if detect_page_under_construction html then
    Lwt_io.printlf "warning: detected page %s as under construction" url >|=
    fun () -> None
  else
    let css_int = find_integer url html in
    let build_year = find_year url html in
    let district = match select_one ".field--name-field-district .field__item" html with
      None -> None
    | Some t -> match leaf_text t with
        None -> None
      | Some v -> Some v in
    let iden = css_int ".field--name-field-vmy-number .field__item" in

    let err_print_table ap = match ap with
      Success table -> return @@ Some table
    | Anomaly reason -> Lwt_io.printlf "weird apartment row skipped at %s: %s" url reason >|= fun () -> None
    | ApartmentError reason -> failwith (sprintf "failed to parse table for %s: %s" url reason) in

    let table = parse_apartment_table html in
    let floor_count = parse_floor_count html in
    Lwt_list.filter_map_s err_print_table table >>= fun apartment_table ->
    return @@ Some { build_year;
      floor_count;
      apartment_table;
      district;
      identifier = iden;
    }

let string_of_apartment_size (ap : apartment_size) : (string * string * string) =
  match ap with
      Same t -> (string_of_float t, "", "")
    | Varying(left, right) -> ("", string_of_float left, string_of_float right)

let string_of_int_range (range : int_or_int_range) : (string * string * string) =
  match range with
      Uniform u -> (string_of_int u, "", "")
    | MinMax(min, max) -> ("", string_of_int min, string_of_int max)

let serialize_houses (houses : parsed_house list) : string list list =
  let to_row (house : parsed_house) =
    List.map (fun (apartment : apartment) ->
      let { residence_type; sizes; count; rent } = apartment in
      let rent_exact, rent_min, rent_max = string_of_int_range rent in
      let uniform_size, min_size, max_size = string_of_apartment_size sizes in
      [ residence_type;
        uniform_size;
        min_size;
        max_size;
        string_of_int count;
        rent_exact;
        rent_min;
        rent_max
      ]
    ) house.apartment_table in

  let apartments = List.map to_row houses in
  List.flatten apartments

let main : unit Lwt.t =
  crawl_all_pages initial_page >>= fun house_links ->
  Lwt_io.printlf "crawling %d house pages..." (List.length house_links) >>= fun () ->
  Lwt_list.map_s fetch_house house_links >>= fun houses ->
  let file = "out.csv" in
  Csv.save file (serialize_houses (cat_maybes houses));
  Lwt_io.printlf "output written to %s" file

let run () = Lwt_main.run main
