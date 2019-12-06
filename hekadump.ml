open Lwt
open Cohttp_lwt_unix
open Soup
open Printf

type pagination_crawl_result =
  { apartment_links : string list;
    next_page       : string option;
  }

type parsed_apartment =
  { build_year  : int;
    floor_count : int;
  }

let parse_next_page (page_soup : soup node) : string option =
  let element = select_one "li.pager__item--next a" page_soup in
  match element with
      Some el -> attribute "href" el
    | None -> None

let parse_page_apartments (page_soup : soup node) : string list =
  let link_elements = select ".node--type-kiinteisto h4 a" page_soup in
  let maybe_links = List.map (attribute "href") (to_list link_elements) in
  let unsafe_filter el = match el with
      Some a -> a
    | None -> failwith "link without href! CSS selector is faulty" in
  List.map unsafe_filter maybe_links

let fetch_links (url : string) : pagination_crawl_result Lwt.t =
  Client.get (Uri.of_string url) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  let soup = parse body in
  { apartment_links = parse_page_apartments soup;
    next_page = parse_next_page soup
  }

let parse_build_year (apartment_html : soup node) : int option =
  let element = select_one ".field--name-field-year-built .field__item" apartment_html in
  match element with
      None -> None
    | Some e -> match leaf_text e with
        None -> None
      | Some text -> int_of_string_opt text

let parse_floor_count (apartment_html : soup node) : int option =
  let element = select_one ".field--name-field-num-floors-min .field__item" apartment_html in
  match element with
      None -> None
    | Some e -> match leaf_text e with
         None -> None
       | Some text -> int_of_string_opt text

let fetch_apartment (url : string) : parsed_apartment Lwt.t =
  Client.get (Uri.of_string url) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  let html = parse body in
  let year = match parse_build_year html with
      Some y -> y
    | None -> failwith (sprintf "invalid build year for %s" url) in

  let floor_count = match parse_floor_count html with
      Some count -> count
    | None -> failwith (sprintf "invalid floor count for %s" url) in

  { build_year = year;
    floor_count = floor_count;
  }

let () =
  let apartment_result = Lwt_main.run (fetch_apartment "https://www.hekaoy.fi/fi/asunnot/kohteet/vuosaarentie-6") in
  let print_floor_count apartment = printf "floors: %d\n" apartment.floor_count in
  print_floor_count apartment_result;
  let crawl_result = Lwt_main.run (fetch_links "https://www.hekaoy.fi/fi/asunnot/kohteet") in
  let print_result apartment = printf "apartment: %s\n" apartment in
  List.iter print_result crawl_result.apartment_links
