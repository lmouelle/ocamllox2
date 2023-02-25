open OUnit2
open Ocamllox2.Eval
open Ocamllox2.Ast

let test_eval_numeric_constant _ =
  assert_equal { res = Number 0.0; new_env = [] } (eval [] (Value (Number 0.0)))

let suite =
  "Eval tests" >::: [ "Numeric Constants" >:: test_eval_numeric_constant ]

let _ = run_test_tt_main suite
