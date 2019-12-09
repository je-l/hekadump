let test_uniform_floor_count () =
  match Hekadump.parse_floor_text "123" with
    | Some (Hekadump.Uniform t) -> Alcotest.(check int) "floor count" 123 t
    | _ -> Alcotest.fail "unable to parse"

let test_min_max_floor_count () =
  match Hekadump.parse_floor_text "4 – 7" with
    | Some (Hekadump.MinMax (l, r)) ->
        Alcotest.(check int) "max floor" 7 r;
        Alcotest.(check int) "min floor" 4 l;
    | _ -> Alcotest.fail "unable to parse minmax"

let () =
  Alcotest.run "Hekadump"
    [
      ( "parse_floor_text",
        [ Alcotest.test_case "uniform" `Quick test_uniform_floor_count
        ; Alcotest.test_case "uniform" `Quick test_min_max_floor_count]
      )
    ]
