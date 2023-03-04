open OUnit2
open Ocamllox2.Ast

let test_location =
  Lexing.{ pos_cnum = 0; pos_lnum = 0; pos_fname = "test"; pos_bol = 0 }

let test_value_to_string _ =
  assert_equal "0." (value_to_string (Number 0.));
  assert_equal "1." (value_to_string (Number 1.));
  assert_equal "1000.1" (value_to_string (Number 1000.1));
  assert_equal "-0.25" (value_to_string (Number (-0.25)));
  assert_equal "\"foo\"" (value_to_string (String "foo"));
  assert_equal "\"\"" (value_to_string (String ""));
  assert_equal "true" (value_to_string (Boolean true));
  assert_equal "false" (value_to_string (Boolean false));
  assert_equal "nil" (value_to_string Nil);
  assert_equal "$foo" (value_to_string (Variable "foo"));
  let closure =
    Closure
      ( [ "i" ],
        Expression
          ( test_location,
            Plus
              ( test_location,
                Value (test_location, Variable "i"),
                Value (test_location, Number 1.) ) ),
        [] )
  in
  assert_equal "fun(i){+($i,1.)}" (value_to_string closure)

let test_env_to_string _ =
  assert_equal "" (env_to_string []);
  assert_equal "(foo , $bar) ; (bar , 1.)"
    (env_to_string [ ("foo", Variable "bar"); ("bar", Number 1.) ]);
  assert_equal "(bar , nil)" (env_to_string [ ("bar", Nil) ])

let test_expr_or_to_string _ =
  let lhs =
    Equals
      ( test_location,
        Value (test_location, Variable "foo"),
        Value (test_location, Number 1.) )
  in
  let rhs = Value (test_location, Boolean false) in
  let item = Or (test_location, lhs, rhs) in
  assert_equal "Or(==($foo,1.),false)" (expr_to_string item)

let test_expr_and_to_string _ =
  let lhs = Value (test_location, Boolean true) in
  let multiply_lhs = Value (test_location, Number 1.) in
  let multiply_rhs = Value (test_location, Number 0.) in
  let rhs =
    Equals
      ( test_location,
        Multiply (test_location, multiply_lhs, multiply_rhs),
        Value (test_location, Number 0.) )
  in
  let item = And (test_location, lhs, rhs) in
  assert_equal "And(true,==(*(1.,0.),0.))" (expr_to_string item)

let test_expr_to_string _ =
  assert_equal (value_to_string Nil)
    (expr_to_string (Value (test_location, Nil)));
  assert_equal
    (value_to_string @@ Number 1.)
    (expr_to_string (Value (test_location, Number 1.)));
  assert_equal
    (value_to_string @@ Variable "foo")
    (expr_to_string (Value (test_location, Variable "foo")));

  test_expr_or_to_string ();
  (* TODO: Write tests for all constructors of the Expr type.
     So incredibly tedious. Consider maybe replacing your *_to_string functions
     with a serialization lib instead? *)
  test_expr_and_to_string ()

let suite =
  "To String tests"
  >::: [
         "Value to String" >:: test_value_to_string;
         "Env to String" >:: test_env_to_string;
         "Expr to String" >:: test_expr_to_string;
       ]

let _ = run_test_tt_main suite
