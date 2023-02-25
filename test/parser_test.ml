open OUnit2
open Ocamllox2.Parse_utils

let test_parse_empty_program _ =
  assert_equal (parse_string "") []

let suite =
  "Parser tests" >::: [
      "Programs" >:: test_parse_empty_program
    ]

let _ = run_test_tt_main suite