open Transform
open Parser

exception InvalidExpression of string
exception UnexpectedValue of string

let value_to_string = function
| NumberValue n -> string_of_int n
| BoolValue b -> string_of_bool b
| NilValue -> "nil"
| SymbolValue s -> s

let rec eval s = 
  match Parser.parse_with expression_parser s with
  | Error msg -> raise @@ InvalidExpression msg
  | Ok e ->
    let ast = transform_expression e in
    eval_ast ast

and eval_ast = function
| ValueAst v -> v
| EqualsAst (lhs, rhs) ->
  begin
    let lhs' = eval_ast lhs in 
    let rhs' = eval_ast rhs in
    match lhs', rhs' with
    | NumberValue n1, NumberValue n2 ->  BoolValue (n1 = n2)
    | BoolValue b1, BoolValue b2 -> BoolValue (b1 = b2)
    | SymbolValue s1, SymbolValue s2 -> BoolValue (s1 = s2)
    | NilValue, NilValue -> BoolValue (true)
    | _ -> raise @@ InvalidExpression "Incompaitible types for equality"
  end
| NotEqualsAst (lhs, rhs) -> 
  begin
    match eval_ast (EqualsAst(lhs, rhs)) with
    | BoolValue b -> BoolValue (not b)
    | _ -> raise @@ failwith "Somehow eval ast made a broken ast"
  end
| PlusAst (lhs, rhs) ->
  begin
    let lhs' = eval_ast lhs in
    let rhs' = eval_ast rhs in 
    match (lhs', rhs') with
    | NumberValue n1, NumberValue n2 -> NumberValue (n1 + n2)
    | SymbolValue s1, SymbolValue s2 -> SymbolValue (s1 ^ s2)
    | _ -> raise @@ InvalidExpression "Incompatible types for addition"
  end
| MinusAst (lhs, rhs ) ->
  begin
    let lhs' = eval_ast lhs in
    let rhs' = eval_ast rhs in
    match (lhs', rhs') with
    | NumberValue n, NumberValue n1 -> NumberValue (n - n1)
    | _ -> raise @@ InvalidExpression "Incompatible types for subtraction"
  end
| DivideAst (lhs, rhs) ->
  begin
    let lhs' = eval_ast lhs in 
    let rhs' = eval_ast rhs in
    match (lhs', rhs') with
    | NumberValue n, NumberValue n1 -> NumberValue (n / n1)
    | _ -> raise @@ InvalidExpression "Incompatible types for division"
  end
| MultiplyAst (lhs, rhs) ->
  begin
    let lhs' = eval_ast lhs in 
    let rhs' = eval_ast rhs in
    match lhs', rhs' with
    | NumberValue n1, NumberValue n2 -> NumberValue (n1 * n2)
    | _ -> raise @@ InvalidExpression "Incompatible types for multiplication"
  end
| GreaterAst (lhs, rhs) ->
  begin
    let lhs' = eval_ast lhs in
    let rhs' = eval_ast rhs in
    match lhs', rhs' with
    | NumberValue n1, NumberValue n2 -> BoolValue (n1 > n2)
    | _ -> raise @@ InvalidExpression "Incompatible types for greater than check"
  end
| LesserAst (lhs, rhs ) ->
  begin
    let lhs' = eval_ast lhs in
    let rhs' = eval_ast rhs in
    match lhs', rhs' with
    | NumberValue n1, NumberValue n2 -> BoolValue (n1 < n2)
    | _ -> raise @@ InvalidExpression "Incompatible types for lesser than check"
  end 
| NotAst e -> 
  begin
    match eval_ast e with
    | BoolValue b -> BoolValue (not b)
    | _ -> raise @@ InvalidExpression "Incompatible types for boolean negation"
  end
| NegateAst n ->
  begin
    match eval_ast n with
    | NumberValue n0 -> NumberValue (- n0)
    | _ -> raise @@ InvalidExpression "Incompatible types for numeric negation"
  end