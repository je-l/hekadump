open Hekadump.Parse
open Hekadump.Strings

let test_uniform_floor_count () =
  match parse_floor_text "123" with
    | Some (Uniform t) -> Alcotest.(check int) "floor count" 123 t
    | Some (MinMax(_, _)) | None -> Alcotest.fail "unable to parse"

let test_min_max_floor_count () =
  match parse_floor_text "4 – 7" with
    | Some (MinMax (l, r)) ->
        Alcotest.(check int) "max floor" 7 r;
        Alcotest.(check int) "min floor" 4 l;
    | Some (Uniform(_)) -> Alcotest.fail "invalid uniform"
    | None -> Alcotest.fail "unable to parse minmax"

let test_min_max_rent () =
  match parse_floor_text "1 100 - 1 200" with
    | Some (MinMax (l, r)) ->
        Alcotest.(check int) "max rent" 1200 r;
        Alcotest.(check int) "min rent" 1100 l;
    | Some (Uniform(_)) | None -> Alcotest.fail "cannot parse rent"

let test_apartment_count () =
  (match parse_apartment_count "12 kpl" with
    Some l -> Alcotest.(check int) "legit apartment count" 12 l
  | None -> Alcotest.fail "cannot parse legit apt count");
  match parse_apartment_count "öööö" with
    Some _ -> Alcotest.fail "should not parse odd apt count"
    | None -> ()

let test_year_parse_normal () =
  match parse_build_year "1950" with
    None -> Alcotest.fail "cannot parse valid build year"
    | Some y -> Alcotest.(check int) "parsing normal build year" 1950 y

(* year parsing should only result in None for certain weird labels, not for
all unexpected inputs *)
let test_year_parse_invalid () =
  match parse_build_year "1950 ja 1970" with
    None -> ()
    | Some _ -> Alcotest.fail "should not parse year range"

let test_apartment_type_parsing () =
  let apartment_print formatter a =
    Format.fprintf
    formatter
    "%s"
    (show_apartment_parse_result a) in

  let apartment = Alcotest.testable apartment_print ( = ) in

  let testcases (raw_text, expected) =
    Alcotest.check
      apartment
      "expected apartment type"
      expected
      (parse_apartment_type raw_text) in

  let type_texts =
    [ ("2h + k", Ok { room_count = 2; features = [Keittio]})
    ; ("15h +   kt", Ok { room_count = 15; features = [KeittoTila]})
    ; ("4h+kt", Ok { room_count = 4; features = [KeittoTila]})
    ; ("6h+ tupak", Ok { room_count = 6; features = [TupaKeittio]})
    ; ("1h+ k (inva)", Ok { room_count = 1; features = [InvaKeittio]})
    ; ("4 h + k + s", Ok { room_count = 4; features = [Keittio; Sauna]})
    ; ("2 h+k+khh", Ok { room_count = 2; features = [Keittio; KodinHoitoHuone]})
    ] in
  List.iter testcases type_texts

let () =
  Alcotest.run "Hekadump"
    [
      ( "regex parsing",
        [Alcotest.test_case "uniform floors" `Quick test_uniform_floor_count;
        Alcotest.test_case "minmax floors" `Quick test_min_max_floor_count;
        Alcotest.test_case "minmax rent" `Quick test_min_max_rent;
        Alcotest.test_case "apartment count" `Quick test_apartment_count;
        Alcotest.test_case "regular year parsing" `Quick test_year_parse_normal;
        Alcotest.test_case "year range parsing" `Quick test_year_parse_invalid;
        Alcotest.test_case "apartment type" `Quick test_apartment_type_parsing;
        ]
      )
    ]
