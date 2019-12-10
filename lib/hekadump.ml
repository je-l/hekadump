open Lwt
open Cohttp_lwt_unix
open Soup
open Printf
open Re

let initial_page = "https://www.hekaoy.fi/fi/asunnot/kohteet"
let root_page = "https://www.hekaoy.fi"

let debug = true

(** Single page, e.g. https://www.hekaoy.fi/fi/asunnot/kohteet?page=3 *)
type pagination_crawl_result =
  { house_links : string list;
    next_page   : string option;
  }

type exact_or_varying = Uniform of int | MinMax of int * int


(** Some houses list floor count as range, e.g. 3-4 *)
type floor_count = exact_or_varying

(** Single house building, e.g. https://www.hekaoy.fi/fi/asunnot/kohteet/hilda-flodinin-kuja-2 *)
type parsed_house =
  { build_year  : int;
    floor_count : floor_count;
    identifier  : int;
    district    : string;
  }

type apartment_size = Same of float | Varying of float * float

type rent = exact_or_varying

(** Single row in the apartment "asuntojakauma" table *)
type apartment =
  { residence_type : string;
    sizes          : apartment_size;
    count          : int;
    rent           : rent;
  }

let parse_apartment_table_row (n : element node) : string =
  let cells = select "td" n in
  match to_list cells with
      [typ; _; _; _] -> (match leaf_text typ with Some t -> t | None -> failwith "unexpected empty table cell")
    | _ -> failwith "unexpected column count"

let parse_apartment_table (html : soup node) : string list =
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

let fetch_links (url : string) : pagination_crawl_result Lwt.t =
  Client.get (Uri.of_string url) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  let html = parse body in
  { house_links = parse_page_houses html;
    next_page = parse_next_page html
  }

(** Extract house links from all pages, until no more pages are left. *)
let rec crawl_all_pages (url : string) : string list =
  (* TODO remove Lwt_main.run *)
  let result = Lwt_main.run (fetch_links url) in

  match result.next_page with
      None -> result.house_links
    | Some next_page -> if debug
                        then result.house_links
                        else result.house_links @ crawl_all_pages next_page

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

let parse_floor_text (text : string) : floor_count option =
  let regex = Pcre.regexp "(\\d+) – (\\d+)" in
  let groups = try Some (Pcre.extract ~rex:regex text)
    with Not_found -> None in

  match groups with
    | Some [| _; left; right|] ->
        Some (MinMax (int_of_string left, int_of_string right))
    | _ -> match int_of_string_opt text with
        None -> None
      | Some e -> Some (Uniform e)

(** Floor count CSS classes are very misleading: either one of these two is
used. The text content might look something like "4 - 6" *)
let parse_floor_count (html : soup node) : floor_count option =
  let element = match select_one ".field--name-field-num-floors-min .field__item" html with
    None -> (match select_one ".field--name-field-num-floors-max .field__item" html with
        None -> None
      | Some e -> Some e)
    | Some t -> Some t in

  match element with
      None -> None
    | Some e -> match leaf_text e with
        None -> None
      | Some t -> parse_floor_text t

let fetch_house (url : string) : parsed_house Lwt.t =
  printf "parsing for url %s\n" url;
  Client.get (Uri.of_string url) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  let html = parse body in
  let css_string = find_string html in
  let css_int = find_integer html in
  let build_year = css_int ".field--name-field-year-built .field__item" in
  let district = css_string ".field--name-field-district .field__item" in
  let iden = css_int ".field--name-field-vmy-number .field__item" in

  let table = parse_apartment_table html in

  List.iter (fun a -> printf "val: %s\n" a) table;

  let floor_count = match parse_floor_count html with
      None -> failwith (sprintf "cannot parse floor count for %s" url)
    | Some t -> t in

  { build_year;
    floor_count = floor_count;
    district = district;
    identifier = iden;
  }

let run () =
  let house_links = crawl_all_pages initial_page in

  (* fetch all houses in single page concurrently ! *)
  let houses = Lwt_main.run (Lwt_list.map_p fetch_house house_links) in
  printf "%d\n" (List.length houses);
