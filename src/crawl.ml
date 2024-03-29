open Cohttp_lwt_unix
open Soup
open Printf

open Parse

let (>>=), (>|=), return = Lwt.((>>=), (>|=), return)

(* debug = only fetching the first page of houses *)
let debug = false

let output_file = "heka_crawl.csv"

let columns =
  [ "address";
    "residence_type";
    "apartment_size_exact";
    "apartment_size_minimum";
    "apartment_size_maximum";
    "apartment_count";
    "rent_exact";
    "rent_minimum";
    "rent_maximum";
    "build_year";
    "floor_count_exact";
    "floor_count_minimum";
    "floor_count_maximum";
    "identifier";
    "district";
    "url";
    "latitude";
    "longitude"
  ]

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

(** Extract house links from all pages, until no more pages are left.

@return list of pages with list of links of houses
*)
let rec crawl_all_pages (url : string) : string list list Lwt.t =
  Lwt_io.printlf "crawling page %s" url >>= fun () ->
  fetch_links url >>= fun result ->
  let { house_links; next_page } = result in

  match next_page with
      None -> return [house_links]
    | Some next ->
        if debug then return [house_links]
        else crawl_all_pages next >|= fun next -> house_links :: next

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

let find_integer_opt html selector =
  let text = select_one selector html in
  match text with
    None -> None
    | Some t -> match leaf_text t with
        None -> None
      | Some e -> int_of_string_opt e

(* There are few housings with build years like "1950-1960". Skip those for now.
TODO: parse the starting year or compute average? *)
let find_year html =
  match select_one ".field--name-field-year-built .field__item" html with
  | None -> None
  | Some n -> match leaf_text n with
    | None -> None
    | Some t -> parse_build_year t

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
    let build_year = find_year html in
    let district = find_district html in
    let opt_identifier =
      find_integer_opt html ".field--name-field-vmy-number .field__item" in

    match opt_identifier with
      None -> Lwt_io.printlf "missing identifier for: %s" url >|= fun () -> None
    | Some identifier ->
      let address = find_string url html "div.kiinteiston-nimi span" in

      let err_print_table ap = match ap with
        Success table -> return @@ Some table
      | Anomaly reason ->
          Lwt_io.printlf
            "weird apartment row skipped at %s: %s"
            url
            reason >|= fun () -> None
      | ApartmentError reason ->
          failwith (sprintf "failed to parse table for %s: %s" url reason) in

      let latitude = parse_latitude html in
      let longitude = parse_longitude html in

      let table = parse_apartment_table html in
      let floor_count = parse_floor_count html in
      Lwt_list.filter_map_s err_print_table table >>= fun apartment_table ->
      return @@ Some { build_year;
        floor_count;
        apartment_table;
        district;
        identifier;
        url;
        address;
        latitude;
        longitude;
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

let string_of_opt_int n = match n with Some t -> string_of_int t | None -> ""

let serialize_houses (houses : parsed_house list) : string list list =
  let to_row (house : parsed_house) =
  let { build_year;
        floor_count;
        identifier;
        district;
        apartment_table;
        url;
        address;
        latitude;
        longitude;
      } = house in

    let apartment_list apt =
      let { residence_type; sizes; count; rent } = apt in
      let rent_exact, rent_min, rent_max = string_of_int_range rent in
      let floors_exact, floors_min, floors_max = match floor_count with
        Some f -> string_of_int_range f
      | None -> "", "", "" in

      let uniform_size, min_size, max_size = string_of_size sizes in
      [ address;
        residence_type;
        uniform_size;
        min_size;
        max_size;
        string_of_int count;
        rent_exact;
        rent_min;
        rent_max;
        string_of_opt_int build_year;
        floors_exact;
        floors_min;
        floors_max;
        string_of_int identifier;
        (match district with Some d -> d | None -> "");
        url;
        string_of_float latitude;
        string_of_float longitude;
      ] in
    List.map apartment_list apartment_table in

  let apartments = List.map to_row houses in
  List.flatten apartments

let main : unit Lwt.t =
  Lwt_io.printl "looking up all house links..." >>= fun () ->
  crawl_all_pages initial_page >>= fun house_links ->

  Lwt_io.printl "crawling into house links..." >>= fun () ->

  (* Fetch all houses in parallel for single pagination *)
  let fetch_parallel page_of_houses =
    Lwt_list.map_p fetch_house page_of_houses in

  Lwt_list.map_s fetch_parallel house_links >>= fun house_batches ->

  let houses = cat_maybes (List.flatten house_batches) in
  let all_rows = columns :: serialize_houses houses in

  assert (List.length all_rows > 0);

  Csv.save output_file all_rows;
  Lwt_io.printlf "output written to %s" output_file

let run () = Lwt_main.run main
