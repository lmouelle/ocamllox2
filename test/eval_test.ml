open OUnit2
open Ocamllox2.Eval
open Ocamllox2.Ast

let test_eval_numeric_constant _ =
  assert_equal { res = Number 0.0; new_env = [] } (eval [] (Value (Number 0.0)))

let test_value_to_string _ =
  assert_equal "0." (value_to_string (Number 0.));
  assert_equal "1." (value_to_string (Number 1.));
  assert_equal "1000.1" (value_to_string (Number 1000.1));
  assert_equal "-0.25" (value_to_string (Number (-0.25)));
  assert_equal "foo" (value_to_string (String "foo"));
  assert_equal "" (value_to_string (String ""));
  assert_equal "true" (value_to_string (Boolean true));
  assert_equal "false" (value_to_string (Boolean false));
  assert_equal "nil" (value_to_string Nil)

let suite =
  "Eval tests"
  >::: [
         "Numeric Constants" >:: test_eval_numeric_constant;
         "Value to String" >:: test_value_to_string;
       ]

let _ = run_test_tt_main suite
