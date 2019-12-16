open Hekadump.Parse

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


let () =
  Alcotest.run "Hekadump"
    [
      ( "regex parsing",
        [Alcotest.test_case "uniform floors" `Quick test_uniform_floor_count;
        Alcotest.test_case "minmax floors" `Quick test_min_max_floor_count;
        Alcotest.test_case "minmax rent" `Quick test_min_max_rent;
        Alcotest.test_case "apartment count" `Quick test_apartment_count
        ]
      )
    ]
