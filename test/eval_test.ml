open OUnit2
open Ocamllox2.Eval
open Ocamllox2.Ast

let test_location =
  Lexing.{ pos_cnum = 0; pos_lnum = 0; pos_fname = "test"; pos_bol = 0 }

let test_eval_values _ =
  assert_equal ~msg:"Should return numeric constants as given"
    { res = Number 0.0; new_env = [] }
    (eval [] (Value (test_location, Number 0.0)));
  assert_equal ~msg:"Should return numeric constants as given, even negative"
    { res = Number (-1.5); new_env = [] }
    (eval [] (Value (test_location, Number (-1.5))));
  assert_equal ~msg:"Should return the environment given"
    { res = Number (-0.); new_env = [ ("foo", String "bar") ] }
    (eval [ ("foo", String "bar") ] (Value (test_location, Number (-0.))));
  assert_equal ~msg:"Should return string constants as given"
    { res = String ""; new_env = [] }
    (eval [] (Value (test_location, String "")));
  assert_equal ~msg:"Should return string constants as given, even empty"
    { res = String "foo"; new_env = [] }
    (eval [] (Value (test_location, String "foo")));
  assert_equal ~msg:"Should return nil as nil"
    { res = Nil; new_env = [] }
    (eval [] (Value (test_location, Nil)));
  assert_raises ~msg:"Should throw when var is undefined"
    (EvalError ("No var defined with name bar", test_location))
    (fun _ -> eval [] (Value (test_location, Variable "bar")));
  assert_raises ~msg:"Should throw when trying to eval var named empty string"
    (EvalError ("Empty string is not valid for var name", test_location))
    (fun _ -> eval [ ("", Number 0.) ] (Value (test_location, Variable "")));
  assert_equal
    ~msg:"Should chase variable references until they resolve to constant"
    { res = Nil; new_env = [ ("foo", Variable "bar"); ("bar", Nil) ] }
    (eval
       [ ("foo", Variable "bar"); ("bar", Nil) ]
       (Value (test_location, Variable "foo")))

let test_value_to_string _ =
  assert_equal "0." (value_to_string (Number 0.));
  assert_equal "1." (value_to_string (Number 1.));
  assert_equal "1000.1" (value_to_string (Number 1000.1));
  assert_equal "-0.25" (value_to_string (Number (-0.25)));
  assert_equal "\"foo\"" (value_to_string (String "foo"));
  assert_equal "\"\"" (value_to_string (String ""));
  assert_equal "true" (value_to_string (Boolean true));
  assert_equal "false" (value_to_string (Boolean false));
  assert_equal "nil" (value_to_string Nil)

let suite =
  "Eval tests"
  >::: [
         "Eval Values" >:: test_eval_values;
         "Value to String" >:: test_value_to_string;
       ]

let _ = run_test_tt_main suite
