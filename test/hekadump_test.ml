let test_uniform_floor_count () =
  match Hekadump.parse_floor_text "123" with
    | Some (Hekadump.Uniform t) -> Alcotest.(check int) "floor count" 123 t
    | Some (Hekadump.MinMax(_, _)) | None -> Alcotest.fail "unable to parse"

let test_min_max_floor_count () =
  match Hekadump.parse_floor_text "4 – 7" with
    | Some (Hekadump.MinMax (l, r)) ->
        Alcotest.(check int) "max floor" 7 r;
        Alcotest.(check int) "min floor" 4 l;
    | Some (Hekadump.Uniform(_)) -> Alcotest.fail "invalid uniform"
    | None -> Alcotest.fail "unable to parse minmax"

let test_min_max_rent () =
  match Hekadump.parse_floor_text "1 100 - 1 200" with
    | Some (Hekadump.MinMax (l, r)) ->
        Alcotest.(check int) "max rent" 1200 r;
        Alcotest.(check int) "min rent" 1100 l;
    | Some (Hekadump.Uniform(_)) | None -> Alcotest.fail "cannot parse rent"

let () =
  Alcotest.run "Hekadump"
    [
      ( "regex parsing",
        [Alcotest.test_case "uniform floors" `Quick test_uniform_floor_count;
        Alcotest.test_case "minmax floors" `Quick test_min_max_floor_count;
        Alcotest.test_case "minmax rent" `Quick test_min_max_rent
        ]
      )
    ]
