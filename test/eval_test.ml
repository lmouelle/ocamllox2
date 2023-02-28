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

let suite = "Eval tests" >::: [ "Eval Values" >:: test_eval_values ]
let _ = run_test_tt_main suite
