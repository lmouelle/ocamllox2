open OUnit2
open Ocamllox2.Ast
open Ocamllox2.Parse_utils

let test_location =
  Lexing.{ pos_cnum = 0; pos_lnum = 0; pos_fname = "test"; pos_bol = 0 }

let assert_parse_string_equal =
  assert_equal
    ~printer:(fun prog -> List.map expr_to_string prog |> String.concat ";")
      (* TODO: Instead of writing yet another function over expr type comparing all non-location
         members of an expr for equality... Just compare serialized format *)
    ~cmp:(fun a b -> List.map expr_to_string a = List.map expr_to_string b)

let test_parse_empty_program _ = assert_equal (parse_string "") []

let test_parse_numeric_expressions _ =
  assert_parse_string_equal
    [
      Plus
        ( test_location,
          Value (test_location, Number 0.),
          Value (test_location, Number 0.) );
    ]
    (parse_string "0+0")

let suite =
  "Parser tests"
  >::: [
         "Empty program" >:: test_parse_empty_program;
         "Numeric expressions" >:: test_parse_numeric_expressions;
       ]

let _ = run_test_tt_main suite
