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
    identifier  : int;
    district    : string;
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

let find_string (html : soup node) (selector : string) : string =
  match select_one selector html with
      None -> failwith (sprintf "can't find element from DOM with %s" selector)
    | Some e -> match leaf_text e with
        None -> failwith (sprintf "cannot find string text from %s" selector)
      | Some t -> t

let find_integer (html : soup node) (selector : string) : int =
  let text = find_string html selector in
  match int_of_string_opt text with
      None -> failwith (sprintf "cannot parse int from selector %s" selector)
    | Some t -> t


let fetch_apartment (url : string) : parsed_apartment Lwt.t =
  printf "parsing url %s\n" url;
  Client.get (Uri.of_string url) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  let html = parse body in
  let css_string = find_string html in
  let css_int = find_integer html in
  let year = css_int ".field--name-field-year-built .field__item" in
  let floors = css_int ".field--name-field-num-floors-min .field__item" in
  let district = css_string ".field--name-field-district .field__item" in
  let iden = css_int ".field--name-field-vmy-number .field__item" in

  { build_year = year;
    floor_count = floors;
    district = district;
    identifier = iden;
  }

let () =
  let apartment_result = Lwt_main.run (fetch_apartment "https://www.hekaoy.fi/fi/asunnot/kohteet/aarrekuja-1") in
  let print_floor_count apartment = printf "floors: %d\n" apartment.floor_count in
  print_floor_count apartment_result;
  let crawl_result = Lwt_main.run (fetch_links "https://www.hekaoy.fi/fi/asunnot/kohteet") in
  let print_result apartment = printf "apartment: %s\n" apartment in
  List.iter print_result crawl_result.apartment_links;
  printf "district %s\n" apartment_result.district;
