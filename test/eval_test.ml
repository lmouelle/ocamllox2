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

let test_eval_if _ =
  (*
     var foo = 1
     var result = if (foo == 1) { foo + 1 } { foo - 1 }
     assert result == 2

     var result = if (!(foo == 1)) { foo + 1 } { foo - 1 }
     assert result == 0

     assert throws {
      if (nil) { foo + 1 } { foo - 1 }
      if ("not-a-valid-cond") { foo + 1 } { foo - 1 }
      if (foo + 1) { foo + 1 } { foo - 1 }
      if (foo) { foo + 1 } { foo - 1 }
     }
  *)
  let var_name = "foo" in
  let env : env = [ (var_name, Number 1.) ] in
  let variable_foo = Value (test_location, Variable var_name) in
  let literal_one = Value (test_location, Number 1.) in
  let foo_eq_one = Equals (test_location, variable_foo, literal_one) in
  let foo_plus_one = Plus (test_location, literal_one, variable_foo) in
  let foo_minus_one = Subtract (test_location, variable_foo, literal_one) in
  let if_expr = If (test_location, foo_eq_one, foo_plus_one, foo_minus_one) in
  assert_equal
    ~msg:"Complex if should eval to true branch and leave env unchanged"
    { res = Number 2.; new_env = env }
    (eval env if_expr);

  let foo_neq_one = Not (test_location, foo_eq_one) in
  let if_expr = If (test_location, foo_neq_one, foo_plus_one, foo_minus_one) in
  assert_equal
    ~msg:"Complex if should eval to false branch and leave env unchanged"
    { res = Number 0.; new_env = env }
    (eval env if_expr);

  let invalid_cond = Value (test_location, Nil) in
  let if_expr = If (test_location, invalid_cond, foo_plus_one, foo_minus_one) in
  assert_raises ~msg:"Nil value is not valid if condition"
    (EvalError ("Invalid condition type for if expr", test_location))
    (fun _ -> eval env if_expr);

  let if_expr = If (test_location, foo_plus_one, foo_plus_one, foo_minus_one) in
  assert_raises ~msg:"Numeric value is not valid if condition"
    (EvalError ("Invalid condition type for if expr", test_location))
    (fun _ -> eval env if_expr);

  let invalid_cond = Value (test_location, String "not-a-valid-cond") in
  let if_expr = If (test_location, invalid_cond, foo_plus_one, foo_minus_one) in
  assert_raises ~msg:"String value is not valid if condition"
    (EvalError ("Invalid condition type for if expr", test_location))
    (fun _ -> eval env if_expr);

  let invalid_cond = Value (test_location, Variable var_name) in
  let if_expr = If (test_location, invalid_cond, foo_plus_one, foo_minus_one) in
  assert_raises ~msg:"Variable reference is not valid if condition"
    (EvalError ("Invalid condition type for if expr", test_location))
    (fun _ -> eval env if_expr)

let test_eval_numeric_operators _ =
  (* Todo: Test that environment is unmodified by numeric ops
     Test division works, addition works, subtraction works, multiplication works as expected
     Test that doing a numeric op against a non-numeric value fails *)
  let var_name = "foo" in
  let env : env = [ (var_name, Number 10.) ] in
  let addition =
    Plus
      ( test_location,
        Value (test_location, Variable var_name),
        Value (test_location, Number 5.) )
  in
  let subtraction =
    Subtract
      ( test_location,
        Value (test_location, Variable var_name),
        Value (test_location, Number 5.) )
  in
  let division =
    Divide
      ( test_location,
        Value (test_location, Variable var_name),
        Value (test_location, Number 5.) )
  in
  let multiplication =
    Multiply
      ( test_location,
        Value (test_location, Variable var_name),
        Value (test_location, Number 5.) )
  in

  assert_equal ~msg:"Test basic addition works" { res = Number 15.; new_env = env } (eval env addition);
  assert_equal ~msg:"Test basic subtraction works" { res = Number 5.; new_env = env } (eval env subtraction);
  assert_equal ~msg:"Test basic division works" { res = Number 2.; new_env = env } (eval env division);
  assert_equal ~msg:"Test basic multiplication works" { res = Number 50.; new_env = env } (eval env multiplication);

  let invalid_op =
    Plus
      ( test_location,
        Value (test_location, Variable var_name),
        Value (test_location, String "example") )
  in
  assert_raises
    (EvalError ("Invalid operands for numeric binary operation", test_location))
    (fun _ -> eval env invalid_op)

let suite =
  "Eval tests"
  >::: [
         "Eval Values" >:: test_eval_values;
         "Eval If" >:: test_eval_if;
         "Eval numeric operators" >:: test_eval_numeric_operators;
       ]

let _ = run_test_tt_main suite
