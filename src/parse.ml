open Re
open Printf
open Soup
open Strings

type int_or_int_range = Uniform of int | MinMax of int * int

(** Some houses list floor count as range, e.g. 3-4 *)
type floor_count = int_or_int_range

type apartment_size = Same of float | Varying of float * float

type rent = int_or_int_range

let initial_page = "https://www.hekaoy.fi/fi/asunnot/kohteet"
let root_page = "https://www.hekaoy.fi"

(* Skip apartments with these types *)
let skip_apartment t = match t with
  "kehitysvammaisten ryhmäkoti" -> false
  | "Päiväkoti" -> false
  | "Päiväkoti." -> false
  | _ -> true

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
    url             : string;
    address         : string;
  }

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

let parse_build_year (text : string) : int option =
  let re = Pcre.regexp
    "(\\d{4} ?- ?\\d{4}|\\d{4}/\\d{2}|\\d{4} ?, ?\\d{4}|\\d{4} ja \\d{4})" in
  match int_of_string_opt text with
    None ->
      if Pcre.pmatch ~rex:re text
      then None else failwith (sprintf "unexpected build year %s" text)
  | Some y -> Some y

type apartment_feature =
  Alkovi
  | Keittio
  | KeittoKomero
  | KeittoTila
  | KodinHoitoHuone
  | KeittoSyvennys
  | KylpyHuone
  | WhatIsLk
  | TupaKeittio
  | TuuliKaappi
  | InvaKeittoKomero
  | LisaHuone
  | KeittioTaiKeittoTila
  | InvaKeittio
  | Tp
  | KeittoTilaKauttaKaksHoo
  | KSoluasunto
  | KeittoKomeroTaiKeittio
  | TerassiPiha
  | Sauna
  | PeeKirjain
  | VaateHuone
  | Parveke
  | SoluParveke
  | KeittioSolu
  | LisaAs
  | AvoKeittio
  | InvaKeittoTila
  | PTaiTerassi
  | ParvekeKahdessaTasossa
  | Solu
  | Piha
  | MaatasoParveke
  | RuokailuTila
  | TuplaKylpyHuone
  | TuplaKeittio
  | RKirjain
  | TuplaVaateHuone
  | Paivakoti
  | KeittoKomeroTukiAsunto
  | MiniKeittio
  | TyoTila
  | SuihkuHuone
(* TODO: add all others *)
[@@deriving show]

type apartment_type =
  { room_count : int;
    features : apartment_feature list;
  }
[@@deriving show]

type apartment_parse_result = (apartment_type, string) result
[@@deriving show]

(* https://fi.wikipedia.org/wiki/Luettelo_asuntokaupassa_k%C3%A4ytett%C3%A4vist%C3%A4_lyhenteist%C3%A4 *)
let feature_of_str text =
  match String.lowercase_ascii text with
  "alk" -> Alkovi
  | "alkovi" -> Alkovi
  | "k" -> Keittio
  | "kt" -> KeittoTila
  | "parveke(kahdessatasossa)" -> ParvekeKahdessaTasossa
  | "kk" -> KeittoKomero
  | "kph" -> KylpyHuone
  | "kh" -> KylpyHuone
  | "lisäas" -> LisaAs
  | "ksyv" -> KeittoSyvennys
  | "ks" -> KeittoSyvennys
  | "avok" -> AvoKeittio
  | "ktinva" -> InvaKeittoTila
  | "ksolu" -> KeittioSolu
  | "ptaiterassi" -> PTaiTerassi
  | "terassipiha" -> TerassiPiha
  | "tupak" -> TupaKeittio
  | "tupak." -> TupaKeittio
  | "rt" -> RuokailuTila
  | "kkinva" -> InvaKeittoKomero
  | "tpk" -> TupaKeittio
  | "kk/k" -> KeittoKomeroTaiKeittio
  | "k/kk" -> KeittoKomeroTaiKeittio
  | "minik" -> MiniKeittio
  | "tupakeittiö" -> TupaKeittio
  | "k(inva)" -> InvaKeittio
  | "työtila" -> TyoTila
  | "kktukiasunto" -> KeittoKomeroTukiAsunto
  | "k/kt" -> KeittioTaiKeittoTila
  | "k(inva-asunto)" -> InvaKeittio
  | "2kph" -> TuplaKylpyHuone
  | "r" -> RKirjain
  | "maatasoparveke" -> MaatasoParveke
  | "2vh" -> TuplaVaateHuone
  | "kk(inva-asunto)" -> InvaKeittoKomero
  | "kinva" -> InvaKeittio
  | "lk" -> WhatIsLk
  | "lisäh" -> LisaHuone
  | "p" -> PeeKirjain
  | "tk" -> TuuliKaappi
  | "k-2h" -> TuplaKeittio
  | "ksoluasunto" -> KSoluasunto
  | "kt/2h" -> KeittoTilaKauttaKaksHoo
  | "vh" -> VaateHuone
  | "tp" -> Tp
  | "parveke(solu)" -> SoluParveke
  | "khh" -> KodinHoitoHuone
  | "piha" -> Piha
  | "parveke" -> Parveke
  | "solu" -> Solu
  | "sauna" -> Sauna
  | "s" -> Sauna
  | "lh" -> Sauna
  | "sh" -> SuihkuHuone
  | _ -> failwith (sprintf "unknown feature: '%s'\n" text)

let parse_apartment_features text =
  let feature_re = Pcre.regexp "\\d+[hH]?v?\\+(.*)" in
  let groups = try Some (Pcre.extract ~rex:feature_re text) with
    Not_found -> None in

  match groups with
    Some [|_; feature_text|] ->
      let parts = String.split_on_char '+' feature_text in
      Ok (List.map feature_of_str parts)
    | _ -> Error "failed to match regex for apartment features"

let parse_apartment_type text : (apartment_type, string) result =
  let no_whitespace = remove_whitespace text in
  let room_count_re = Pcre.regexp "(\\d+) *[hH]?v?" in
  let groups = try Some (Pcre.extract ~rex:room_count_re no_whitespace) with
    Not_found -> None in

  match groups with
    Some [|_; count_text |] -> begin
      match parse_apartment_features no_whitespace with
        Ok features ->
          Ok { room_count = int_of_string count_text
             ; features = features
             }
      | Error e -> Error (sprintf "failed to parse %s: %s" text e)
    end
    | _ -> Error (sprintf "regex failed for apartment parse: %s" text)
