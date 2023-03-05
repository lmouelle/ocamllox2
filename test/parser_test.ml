open OUnit2
open Ocamllox2.Pretty_print
open Ocamllox2.Parse_utils

let assert_prog_equals =
  assert_equal
    ~printer:(fun p -> program_to_string p)
    ~cmp:(fun a b -> program_to_string a = program_to_string b)

let test_empty_program _ =
  let p = parse_string "" in
  assert_prog_equals p []

let test_simple_nums _ =
  let p = parse_string "1 + 1;" in
  assert_prog_equals p
    [
      Ocamllox2.Ast.Expression
        ( { Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 },
          Ocamllox2.Ast.Plus
            ( { Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 },
              Ocamllox2.Ast.Value
                ( {
                    Lexing.pos_fname = "";
                    pos_lnum = 1;
                    pos_bol = 0;
                    pos_cnum = 0;
                  },
                  Ocamllox2.Ast.Number 1. ),
              Ocamllox2.Ast.Value
                ( {
                    Lexing.pos_fname = "";
                    pos_lnum = 1;
                    pos_bol = 0;
                    pos_cnum = 4;
                  },
                  Ocamllox2.Ast.Number 1. ) ) );
    ]

let test_simple_assignment _ =
  let expected_ast =
    [
      Ocamllox2.Ast.Declaration
        ( { Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 },
          "a",
          Some
            (Ocamllox2.Ast.Value
               ( {
                   Lexing.pos_fname = "";
                   pos_lnum = 1;
                   pos_bol = 0;
                   pos_cnum = 8;
                 },
                 Ocamllox2.Ast.String "foo" )) );
    ]
  in
  let prog = parse_string "var a = \"foo\";" in
  assert_prog_equals expected_ast prog

let suite =
  "Parser tests"
  >::: [
         "Test empty prog" >:: test_empty_program;
         "Test simple assignment" >:: test_simple_assignment;
         "Test simple nums" >:: test_simple_nums
       ]

let _ = run_test_tt_main suite
