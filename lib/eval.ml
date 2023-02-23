open Transform
open Parser

exception InvalidExpression of string
exception UnexpectedValue of string

type eval_result = {
  value : value;
  env : (string * value) list
}

let value_to_string = function
| NumberValue n -> string_of_int n
| BoolValue b -> string_of_bool b
| NilValue -> "nil"
| SymbolValue s -> s

let rec eval s = 
  match Parser.parse_with program_parser s with
  | Error msg -> raise @@ InvalidExpression msg
  | Ok e ->
    let ast = transform_program e in
    eval_ast ast

and eval_ast = function
| ValueAst v -> {value = v; env = []}
| EqualsAst (lhs, rhs) ->
  begin
    let lhs' = eval_ast lhs in 
    let rhs' = eval_ast rhs in
    match lhs'.value, rhs'.value with
    | NumberValue n1, NumberValue n2 -> {value = BoolValue (n1 = n2); env = []}
    | BoolValue b1, BoolValue b2 -> {value = BoolValue (b1 = b2); env = []}
    | SymbolValue s1, SymbolValue s2 -> {value = BoolValue (s1 = s2); env = []}
    | NilValue, NilValue -> {value = BoolValue (true); env = []}
    | _ -> raise @@ InvalidExpression "Incompaitible types for equality"
  end
| NotEqualsAst (lhs, rhs) -> 
  begin
    match eval_ast (EqualsAst(lhs, rhs)) with
    | {value = BoolValue b; _} -> {value = BoolValue (not b); env = []}
    | _ -> raise @@ failwith "Somehow eval ast made a broken ast"
  end
| PlusAst (lhs, rhs) ->
  begin
    let lhs' = eval_ast lhs in
    let rhs' = eval_ast rhs in 
    match (lhs'.value, rhs'.value) with
    | NumberValue n1, NumberValue n2 -> {value = NumberValue (n1 + n2); env = []}
    | SymbolValue s1, SymbolValue s2 -> {value = SymbolValue (s1 ^ s2); env = []}
    | _ -> raise @@ InvalidExpression "Incompatible types for addition"
  end
| MinusAst (lhs, rhs ) ->
  begin
    let lhs' = eval_ast lhs in
    let rhs' = eval_ast rhs in
    match (lhs'.value, rhs'.value) with
    | NumberValue n, NumberValue n1 -> {value = NumberValue (n - n1); env = []}
    | _ -> raise @@ InvalidExpression "Incompatible types for subtraction"
  end
| DivideAst (lhs, rhs) ->
  begin
    let lhs' = eval_ast lhs in 
    let rhs' = eval_ast rhs in
    match (lhs'.value, rhs'.value) with
    | NumberValue n, NumberValue n1 -> {value = NumberValue (n / n1); env = []}
    | _ -> raise @@ InvalidExpression "Incompatible types for division"
  end
| MultiplyAst (lhs, rhs) ->
  begin
    let lhs' = eval_ast lhs in 
    let rhs' = eval_ast rhs in
    match lhs'.value, rhs'.value with
    | NumberValue n1, NumberValue n2 -> {value = NumberValue (n1 * n2); env = []}
    | _ -> raise @@ InvalidExpression "Incompatible types for multiplication"
  end
| GreaterAst (lhs, rhs) ->
  begin
    let lhs' = eval_ast lhs in
    let rhs' = eval_ast rhs in
    match lhs'.value, rhs'.value with
    | NumberValue n1, NumberValue n2 -> {value = BoolValue (n1 > n2); env = []}
    | _ -> raise @@ InvalidExpression "Incompatible types for greater than check"
  end
| LesserAst (lhs, rhs ) ->
  begin
    let lhs' = eval_ast lhs in
    let rhs' = eval_ast rhs in
    match lhs'.value, rhs'.value with
    | NumberValue n1, NumberValue n2 -> {value = BoolValue (n1 < n2); env = []}
    | _ -> raise @@ InvalidExpression "Incompatible types for lesser than check"
  end 
| NotAst e -> 
  begin
    match eval_ast e with
    | {value = BoolValue b; _} -> {value = BoolValue (not b); env = []}
    | _ -> raise @@ InvalidExpression "Incompatible types for boolean negation"
  end
| NegateAst n ->
  begin
    match eval_ast n with
    | {value = NumberValue n0; env = []} -> {value = NumberValue (- n0); env = []}
    | _ -> raise @@ InvalidExpression "Incompatible types for numeric negation"
  end
| PrintStatementAst ast ->
  begin
     let {value; env} = eval_ast ast in
     value_to_string value |> print_string;
     print_newline ();
     {value; env}
  end
| ExpressionStatementAst ast -> eval_ast ast
| ProgramAst asts -> 
  let rec list_last = function 
  | [] -> {value = NilValue; env = []}
  | [x] -> x
  | _ :: xs -> list_last xs 
  in
  List.map eval_ast asts |> list_last
