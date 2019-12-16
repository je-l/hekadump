open Cohttp_lwt_unix
open Soup
open Printf
open Re

open Parse

let (>>=), (>|=), return = Lwt.((>>=), (>|=), return)

(* debug = only fetching the first page of houses *)
let debug = false

let output_file = "heka_crawl.csv"

let cat_maybes (options : 'a option list) : 'a list =
  List.filter_map (fun a -> a) options

let http_get (url : string) : soup node Lwt.t =
  Client.get (Uri.of_string url) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body -> parse body

let fetch_links (url : string) : pagination_crawl_result Lwt.t =
  http_get url >|= fun html ->
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

let find_string
  (page : string)
  (html : soup node)
  (selector : string) : string =
    match select_one selector html with
        None -> failwith (sprintf
          "can't find element from DOM with %s from page %s"
          selector
          page)
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

(* There are few housings with build years like "1950-1960". Skip those for now.
TODO: parse the starting year or compute average? *)
let find_year url html =
  let year = find_string
    url
    html
    ".field--name-field-year-built .field__item" in
  let re = Pcre.regexp
    "(\\d{4} ?- ?\\d{4}|\\d{4}/\\d{2}|\\d{4} ?, ?\\d{4}|\\d{4} ja \\d{4})" in
  match int_of_string_opt year with
    None ->
      if Pcre.pmatch ~rex:re year
      then None else failwith (sprintf "unexpected build year %s" year)
  | Some y -> Some y

(** Floor count CSS classes are misleading: either one of these two is used.
The text content might look something like "4 - 6" *)
let parse_floor_count (html : soup node) : floor_count option =
  let element = match select_one
    ".field--name-field-num-floors-min .field__item"
    html with
      None ->
        (match select_one
          ".field--name-field-num-floors-max .field__item"
          html with
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

let find_district html =
  match select_one ".field--name-field-district .field__item" html with
    None -> None
  | Some t -> match leaf_text t with
      None -> None
    | Some v -> Some v

let fetch_house (url : string) : parsed_house option Lwt.t =
  http_get url >>= fun html ->

  if detect_page_under_construction html then
    Lwt_io.printlf "warning: detected page %s as under construction" url >|=
    fun () -> None
  else
    let css_int = find_integer url html in
    let build_year = find_year url html in
    let district = find_district html in
    let identifier = css_int ".field--name-field-vmy-number .field__item" in

    let err_print_table ap = match ap with
      Success table -> return @@ Some table
    | Anomaly reason ->
        Lwt_io.printlf
          "weird apartment row skipped at %s: %s"
          url
          reason >|= fun () -> None
    | ApartmentError reason ->
        failwith (sprintf "failed to parse table for %s: %s" url reason) in

    let table = parse_apartment_table html in
    let floor_count = parse_floor_count html in
    Lwt_list.filter_map_s err_print_table table >>= fun apartment_table ->
    return @@ Some { build_year;
      floor_count;
      apartment_table;
      district;
      identifier;
    }

let string_of_size (ap : apartment_size) : (string * string * string) =
  match ap with
      Same t -> (string_of_float t, "", "")
    | Varying(left, right) -> ("", string_of_float left, string_of_float right)

let string_of_int_range
  (range : int_or_int_range) : (string * string * string) =
    match range with
        Uniform u -> (string_of_int u, "", "")
      | MinMax(min, max) -> ("", string_of_int min, string_of_int max)

let serialize_houses (houses : parsed_house list) : string list list =
  let to_row (house : parsed_house) =
    List.map (fun (apartment : apartment) ->
      let { residence_type; sizes; count; rent } = apartment in
      let rent_exact, rent_min, rent_max = string_of_int_range rent in
      let uniform_size, min_size, max_size = string_of_size sizes in
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
  Lwt_io.printlf
    "crawling %d house pages..."
    (List.length house_links) >>= fun () ->
      Lwt_list.map_s fetch_house house_links >>= fun houses ->
      Csv.save output_file (serialize_houses (cat_maybes houses));
      Lwt_io.printlf "output written to %s" output_file

let run () = Lwt_main.run main
