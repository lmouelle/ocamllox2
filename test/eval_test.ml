open OUnit2
open Ocamllox2.Eval
open Ocamllox2.Ast

let test_location =
  Lexing.{ pos_cnum = 0; pos_lnum = 0; pos_fname = "test"; pos_bol = 0 }

let assert_eval_result_equal =
  assert_equal ~printer:(fun { res; new_env } ->
    value_to_string res ^ "->:value in env:->" ^ env_to_string new_env)

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

  assert_equal ~msg:"Test basic addition works"
    { res = Number 15.; new_env = env }
    (eval env addition);
  assert_equal ~msg:"Test basic subtraction works"
    { res = Number 5.; new_env = env }
    (eval env subtraction);
  assert_equal ~msg:"Test basic division works"
    { res = Number 2.; new_env = env }
    (eval env division);
  assert_equal ~msg:"Test basic multiplication works"
    { res = Number 50.; new_env = env }
    (eval env multiplication);

  let invalid_op =
    Plus
      ( test_location,
        Value (test_location, Variable var_name),
        Value (test_location, String "example") )
  in
  assert_raises
    (EvalError ("Invalid operands for numeric binary operation", test_location))
    (fun _ -> eval env invalid_op)

let test_eval_or _ =
  let var_name = "foo" in
  let env : env = [ (var_name, String "bar") ] in
  let var_eq_correct =
    Equals
      ( test_location,
        Value (test_location, Variable var_name),
        Value (test_location, String "bar") )
  in
  let true_val = Value (test_location, Boolean true) in
  let false_val = Value (test_location, Boolean false) in
  let invalid_cond = Value (test_location, Nil) in
  let or_val = Or (test_location, var_eq_correct, true_val) in
  assert_equal ~msg:"Test that evals to lhs in simple case"
    { res = Boolean true; new_env = env }
    (eval env or_val);

  let or_val = Or (test_location, false_val, true_val) in
  assert_equal ~msg:"Test that evals to rhs in simple case"
    { res = Boolean true; new_env = env }
    (eval env or_val);

  let or_val = Or (test_location, true_val, false_val) in
  assert_equal ~msg:"Test that it short circuits and evals to lhs"
    { res = Boolean true; new_env = env }
    (eval env or_val);

  let or_val = Or (test_location, false_val, false_val) in
  assert_equal ~msg:"Test that it evals to false if both sides are false"
    { res = Boolean false; new_env = env }
    (eval env or_val);

  let or_val = Or (test_location, true_val, invalid_cond) in
  assert_equal
    ~msg:"Test that it short circuts and does not eval invalid condition"
    { res = Boolean true; new_env = env }
    (eval env or_val);

  let or_val = Or (test_location, invalid_cond, true_val) in
  assert_raises ~msg:"Test that it throws when lhs is invalid"
    (EvalError ("Invalid operands for boolean or", test_location))
    (fun _ -> eval env or_val)

let test_eval_and _ =
  let var_name = "foo" in
  let env : env = [ (var_name, String "bar") ] in
  let var_eq_correct =
    Equals
      ( test_location,
        Value (test_location, Variable var_name),
        Value (test_location, String "bar") )
  in
  let true_val = Value (test_location, Boolean true) in
  let false_val = Value (test_location, Boolean false) in
  let invalid_cond = Value (test_location, Nil) in
  let and_expr = And (test_location, var_eq_correct, true_val) in
  assert_equal ~msg:"Test that basic boolean and works"
    { res = Boolean true; new_env = env }
    (eval env and_expr);

  let and_expr = And (test_location, false_val, true_val) in
  assert_equal ~msg:"Test that basic boolean evals to false"
    { res = Boolean false; new_env = env }
    (eval env and_expr);

  let and_expr = And (test_location, true_val, false_val) in
  assert_equal ~msg:"Test that basic boolean evals to false"
    { res = Boolean false; new_env = env }
    (eval env and_expr);

  let and_expr = And (test_location, false_val, false_val) in
  assert_equal ~msg:"Test that basic boolean evals to false"
    { res = Boolean false; new_env = env }
    (eval env and_expr);

  let and_expr = And (test_location, invalid_cond, true_val) in
  assert_raises ~msg:"Test that invalid and operand throws"
    (EvalError ("Invalid operands for boolean and", test_location))
    (fun _ -> eval env and_expr)

let test_eval_assignment _ =
  let var_name = "foo" in
  let var_string_value = String "bar" in
  let env : env = [] in
  let assign_expr =
    Assignment (test_location, var_name, Value (test_location, var_string_value))
  in
  assert_equal ~msg:"Test that variable declaration works in simple case"
    { res = var_string_value; new_env = [ (var_name, var_string_value) ] }
    (eval env assign_expr);

  let env : env = [ (var_name, var_string_value) ] in
  assert_raises ~msg:"Test that shadowing var defs is disallowed"
    (EvalError
       ("Cannot shadow variables, update var or create new one", test_location))
    (fun _ -> eval env assign_expr)

let test_eval_equality _ =
  let var_expr = Value (test_location, Variable "foo") in
  let nil_expr = Value (test_location, Nil) in
  let num_expr = Value (test_location, Number 0.) in
  let str_expr = Value (test_location, String "bar") in
  let bool_expr = Value (test_location, Boolean true) in
  let env =
    [ ("foo", Number 0.); ("quux", Variable "foo"); ("baz", Variable "quux") ]
  in

  (* Check for standard equality *)
  let equal_expr = Equals (test_location, nil_expr, nil_expr) in
  assert_equal ~msg:"Test equality for self works in simple case, nil"
    { res = Boolean true; new_env = env }
    (eval env equal_expr);

  let equal_expr = Equals (test_location, bool_expr, bool_expr) in
  assert_equal ~msg:"Test equality for self works in simple case, bool"
    { res = Boolean true; new_env = env }
    (eval env equal_expr);

  let equal_expr = Equals (test_location, var_expr, num_expr) in
  assert_equal ~msg:"Test equality for self works and vars are chased"
    { res = Boolean true; new_env = env }
    (eval env equal_expr);

  let equal_expr = Equals (test_location, str_expr, str_expr) in
  assert_equal ~msg:"Test equality for self works and strings compare correctly"
    { res = Boolean true; new_env = env }
    (eval env equal_expr);

  (* Now check for equality is false *)
  let equal_expr =
    Equals (test_location, var_expr, Value (test_location, Number (-1.)))
  in
  assert_equal ~msg:"Test equality for self works and vars are chased"
    { res = Boolean false; new_env = env }
    (eval env equal_expr);

  let equal_expr =
    Equals
      (test_location, str_expr, Value (test_location, String "never-matches"))
  in
  assert_equal ~msg:"Test equality for self works and strings compare correctly"
    { res = Boolean false; new_env = env }
    (eval env equal_expr);

  (* Check for nested variable chasing *)
  let equal_expr =
    Equals (test_location, Value (test_location, Variable "baz"), num_expr)
  in
  assert_equal ~msg:"Test nested vars are chased"
    { res = Boolean true; new_env = env }
    (eval env equal_expr);

  (* Now test that some values throw *)
  let equal_expr = Equals (test_location, nil_expr, num_expr) in
  assert_raises ~msg:"Test that unequal values throw"
    (EvalError ("Cannot compare value to nil", test_location))
    (fun _ -> eval env equal_expr);

  let equal_expr = Equals (test_location, nil_expr, num_expr) in
  assert_raises ~msg:"Test that unequal values throw"
    (EvalError ("Cannot compare value to nil", test_location))
    (fun _ -> eval env equal_expr);

  let equal_expr = Equals (test_location, bool_expr, str_expr) in
  assert_raises ~msg:"Test that unequal values throw"
    (EvalError ("Cannot compare string to bool", test_location))
    (fun _ -> eval env equal_expr);

  (* Test a few inequality cases *)
  let equal_expr =
    NotEquals (test_location, var_expr, Value (test_location, Number (-1.)))
  in
  assert_equal ~msg:"Test inequality works and vars are chased"
    { res = Boolean true; new_env = env }
    (eval env equal_expr);

  let equal_expr =
    NotEquals
      (test_location, str_expr, Value (test_location, String "never-matches"))
  in
  assert_equal ~msg:"Test equality for self works and strings compare correctly"
    { res = Boolean true; new_env = env }
    (eval env equal_expr)

let test_eval_comparison _ =
  let var_expr = Value (test_location, Variable "foo") in
  let nil_expr = Value (test_location, Nil) in
  let num_expr = Value (test_location, Number 0.) in
  let str_expr = Value (test_location, String "bar") in
  let bool_expr = Value (test_location, Boolean true) in
  let env =
    [ ("foo", Number 0.); ("quux", Variable "foo"); ("baz", Variable "quux") ]
  in

  let expr =
    Less (test_location, num_expr, Value (test_location, Variable "baz"))
  in
  assert_equal ~msg:"Test variable resolves to number for comparison, less"
    { res = Boolean false; new_env = env }
    (eval env expr);

  let expr =
    LessEqual (test_location, num_expr, Value (test_location, Variable "baz"))
  in
  assert_equal ~msg:"Test variable resolves to number for comparison, less eq"
    { res = Boolean true; new_env = env }
    (eval env expr);

  let expr =
    Greater
      ( test_location,
        Value (test_location, Number 10.),
        Value (test_location, Variable "baz") )
  in
  assert_equal ~msg:"Test variable resolves to number for comparison, greater"
    { res = Boolean true; new_env = env }
    (eval env expr);

  let expr =
    GreaterEqual (test_location, var_expr, Value (test_location, Variable "baz"))
  in
  assert_equal
    ~msg:"Test variable resolves to number for comparison, greater eq"
    { res = Boolean true; new_env = env }
    (eval env expr);

  let expr =
    GreaterEqual (test_location, nil_expr, Value (test_location, Variable "baz"))
  in
  assert_raises ~msg:"Test throws for invalid values for comparison"
    (EvalError
       ("Only numeric values are valid operands for comparison", test_location))
    (fun _ -> eval env expr);

  let expr =
    GreaterEqual (test_location, str_expr, Value (test_location, Variable "baz"))
  in
  assert_raises ~msg:"Test throws for invalid values for comparison"
    (EvalError
       ("Only numeric values are valid operands for comparison", test_location))
    (fun _ -> eval env expr);

  let expr =
    GreaterEqual
      (test_location, bool_expr, Value (test_location, Variable "baz"))
  in
  assert_raises ~msg:"Test throws for invalid values for comparison"
    (EvalError
       ("Only numeric values are valid operands for comparison", test_location))
    (fun _ -> eval env expr)

let test_eval_not _ =
  let var_expr = Value (test_location, Variable "foo") in
  let nil_expr = Value (test_location, Nil) in
  let num_expr = Value (test_location, Number 0.) in
  let str_expr = Value (test_location, String "bar") in
  let bool_expr = Value (test_location, Boolean true) in
  let env = [ ("foo", Boolean false) ] in
  let expr = Not (test_location, bool_expr) in
  assert_equal ~msg:"Test basic not case"
    { res = Boolean false; new_env = env }
    (eval env expr);

  let expr = Not (test_location, var_expr) in
  assert_equal ~msg:"Test not chases vars"
    { res = Boolean true; new_env = env }
    (eval env expr);

  let expr = Not (test_location, nil_expr) in
  assert_raises ~msg:"Test not throws on nil"
    (EvalError ("Not operator must have boolean operand", test_location))
    (fun _ -> eval env expr);

  let expr = Not (test_location, num_expr) in
  assert_raises ~msg:"Test not throws on num"
    (EvalError ("Not operator must have boolean operand", test_location))
    (fun _ -> eval env expr);

  let expr = Not (test_location, str_expr) in
  assert_raises ~msg:"Test not throws on str"
    (EvalError ("Not operator must have boolean operand", test_location))
    (fun _ -> eval env expr)

let test_eval_function _ =
  let params = [ "a"; "b" ] in
  let body =
    Plus
      ( test_location,
        Value (test_location, Variable "a"),
        Value (test_location, Variable "b") )
  in
  let expr = Function (test_location, params, body) in
  assert_equal ~msg:"Test basic closure construction"
    { res = Closure (params, body, []); new_env = [] }
    (eval [] expr)

let test_eval_mutation _ =
  let var_name = "foo" in
  let new_var_value = Number 1. in
  let env = [ (var_name, Number 0.) ] in
  let expr =
    Mutation (test_location, var_name, Value (test_location, new_var_value))
  in
  assert_eval_result_equal ~msg:"Test basic var mutation"
    { res = new_var_value; new_env = (var_name, new_var_value) :: env }
    (eval env expr);

  let var_name = "baz" in
  let expr =
    Mutation (test_location, var_name, Value (test_location, Number 1.))
  in
  assert_raises ~msg:"Test mutation throws for unknown var"
    (EvalError ("Cannot mutate unknown var " ^ var_name, test_location))
    (fun _ -> eval env expr);

  (* TODO: For now I assert that vars are dynamic: That I can
     change the type of a var by assignment at any time. This may not work
     in the long run if I want to add a type checker. *)
  let var_name = "foo" in
  let new_var_value = String "value" in
  let expr =
    Mutation (test_location, var_name, Value (test_location, new_var_value))
  in
  assert_eval_result_equal ~msg:"Test vars are uni-typed"
    { res = String "value"; new_env = (var_name, new_var_value) :: env }
    (eval env expr)

let suite =
  "Eval tests"
  >::: [
         "Eval Values" >:: test_eval_values;
         "Eval If" >:: test_eval_if;
         "Eval numeric operators" >:: test_eval_numeric_operators;
         "Eval Or" >:: test_eval_or;
         "Eval and" >:: test_eval_and;
         "Eval assignment" >:: test_eval_assignment;
         "Eval equality" >:: test_eval_equality;
         "Eval comparison" >:: test_eval_comparison;
         "Eval not" >:: test_eval_not;
         "Eval function def" >:: test_eval_function;
         "Eval var mutation" >:: test_eval_mutation;
       ]

let _ = run_test_tt_main suite
