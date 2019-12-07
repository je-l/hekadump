open Lwt
open Cohttp_lwt_unix
open Soup
open Printf
(* open Re.Perl *)

let initial_page = "https://www.hekaoy.fi/fi/asunnot/kohteet"
let root_page = "https://www.hekaoy.fi"

let debug = true

type pagination_crawl_result =
  { apartment_links : string list;
    next_page       : string option;
  }

type parsed_apartment =
  { build_year      : int;
    floor_count     : int option;
    floor_count_min : int option;
    floor_count_max : int option;
    identifier      : int;
    district        : string;
  }

let parse_next_page (page_soup : soup node) : string option =
  let element = select_one ".pager__item--next a" page_soup in
  match element with
      None -> None
    | Some el -> match attribute "href" el with
        None -> None
      | Some link -> Some (initial_page ^ link)

let parse_page_apartments (html : soup node) : string list =
  let link_elements = select ".node--type-kiinteisto h4 a" html in
  let maybe_links = List.map (attribute "href") (to_list link_elements) in
  let unsafe_filter el = match el with
      Some a -> a
    | None -> failwith "link without href! CSS selector is faulty" in
  let links = List.map unsafe_filter maybe_links in
  List.map (fun e -> root_page ^ e) links

let fetch_links (url : string) : pagination_crawl_result Lwt.t =
  Client.get (Uri.of_string url) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  let html = parse body in
  { apartment_links = parse_page_apartments html;
    next_page = parse_next_page html
  }

(** Extract apartment links from all pages, until no more pages are left. *)
let rec crawl_all_pages (url : string) : string list =
  let result = Lwt_main.run (fetch_links url) in

  match result.next_page with
      None -> result.apartment_links
    | Some next_page -> if debug
                        then result.apartment_links
                        else result.apartment_links @ crawl_all_pages next_page

let find_string (html : soup node) (selector : string) : string =
  match select_one selector html with
      None -> failwith (sprintf "can't find element from DOM with %s" selector)
    | Some e -> match leaf_text e with
        None -> failwith (sprintf "cannot find string text from %s" selector)
      | Some t -> t

let find_integer_opt html selector =
  let text = find_string html selector in
  int_of_string_opt text

let find_integer html selector =
  let text = find_integer_opt html selector in
  match text with
      None -> failwith (sprintf "cannot parse int from selector %s" selector)
    | Some t -> t

let regex_test (input : string) =
  let regexx = Re.Pcre.regexp "(\\d+)-(\\d+)" in
  Re.Pcre.extract ~rex:regexx input

let fetch_apartment (url : string) : parsed_apartment Lwt.t =
  printf "parsing for url %s\n" url;
  Client.get (Uri.of_string url) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  let html = parse body in
  let css_string = find_string html in
  let css_int = find_integer html in
  let css_int_opt = find_integer_opt html in
  let build_year = css_int ".field--name-field-year-built .field__item" in
  let floors = css_int_opt ".field--name-field-num-floors-min .field__item" in
  let district = css_string ".field--name-field-district .field__item" in
  let iden = css_int ".field--name-field-vmy-number .field__item" in

  let groups = regex_test "123-456" in
  List.iter (printf "a: %s\n") (Array.to_list groups);

  { build_year;
    floor_count = floors;
    district = district;
    identifier = iden;
    floor_count_min = Some 1;
    floor_count_max = Some 2;
  }

let () =
  let apartment_links = crawl_all_pages initial_page in

  (* fetch all apartments in single page concurrently ! *)
  let apartments = Lwt_main.run (Lwt_list.map_p fetch_apartment apartment_links) in
  printf "%d\n" (List.length apartments);
  List.iter (fun a -> printf "floors: %d\n" (match a.floor_count with Some f -> f | None -> -1)) apartments
