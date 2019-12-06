open Lwt
open Cohttp_lwt_unix
open Soup
open Printf

type page_crawl_result =
  { apartment_links : string list;
    next_page       : string option;
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

let fetch_links url =
  Client.get (Uri.of_string url) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  let soup = parse body in
  let link_elements = select ".node--type-kiinteisto h4 a" soup in
  to_list link_elements

let () =
  let links = Lwt_main.run (fetch_links "https://www.hekaoy.fi/fi/asunnot/kohteet") in
  let a_attrs = List.map (attribute "href") links in
  let print el = match el with
    | None -> printf "no href in this link\n"
    | Some a -> printf "link: %s\n" a in
  List.iter print a_attrs
