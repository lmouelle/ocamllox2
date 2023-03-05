open OUnit2
open Ocamllox2.Pretty_print
open Ocamllox2.Parse_utils

let assert_equals_prog =
  assert_equal
    ~printer:(fun p -> program_to_string p)
    ~cmp:(fun a b -> program_to_string a = program_to_string b)

let test_empty_program _ =
  let p = parse_string "" in
  assert_equals_prog p []

let suite = "Parser tests" >::: [ "Test empty prog" >:: test_empty_program ]
let _ = run_test_tt_main suite
