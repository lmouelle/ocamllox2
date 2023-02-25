open Ast

exception EvalError of string

type eval_result = {
  res : value; 
  new_env : (string * value) list;
}

let value_to_string = function
| Boolean b -> string_of_bool b
| Variable s | String s -> s
| Number n -> string_of_float n
| Nil -> "nil"

let rec eval env = function
| Value v -> {res = v; new_env = env}
| If(cond, iftrue, iffalse) ->
  begin
    let result = eval env cond in
    match result.res with 
    | Boolean true -> eval env iftrue
    | Boolean false -> eval env iffalse
    | Number _ | String _ | Nil | Variable _ -> raise @@ EvalError "Invalid condition type for if expr"
  end
| Plus (lhs, rhs) ->
  let lhs_result = eval env lhs in
  let rhs_result = eval env rhs in
  begin
    match lhs_result.res, rhs_result.res with
    | Number n1, Number n2 -> {res = Number (Float.add n1 n2); new_env = env}
    | _, _ -> (* TODO  instead of wildcard case match list all possible matches for better type safety *)
    raise @@ EvalError "Invalid operands for addition"
  end
| Multiply (lhs, rhs) ->
  let lhs_result = eval env lhs in
  let rhs_result = eval env rhs in
  begin
    match lhs_result.res, rhs_result.res with
    | Number n1, Number n2 -> {res = Number (Float.mul n1 n2); new_env = env}
    | _, _ -> (* TODO  instead of wildcard case match list all possible matches for better type safety *)
    raise @@ EvalError "Invalid operands for multiplication"
  end
| Subtract (lhs, rhs) ->
  let lhs_result = eval env lhs in
  let rhs_result = eval env rhs in
  begin
    match lhs_result.res, rhs_result.res with
    | Number n1, Number n2 -> {res = Number (Float.sub n1 n2); new_env = env}
    | _, _ -> (* TODO  instead of wildcard case match list all possible matches for better type safety *)
    raise @@ EvalError "Invalid operands for subtraction"
  end    
| Divide (lhs, rhs) ->
  let lhs_result = eval env lhs in
  let rhs_result = eval env rhs in
  begin
    match lhs_result.res, rhs_result.res with
    | Number n1, Number n2 -> {res = Number (Float.div n1 n2); new_env = env}
    | _, _ -> (* TODO  instead of wildcard case match list all possible matches for better type safety *)
    raise @@ EvalError "Invalid operands for division"
  end      
| Or (lhs, rhs) ->
  let lhs_result = eval env lhs in
  begin 
    match lhs_result.res with
    | Boolean true -> { res = Boolean true; new_env = env }
    | Boolean false -> eval env rhs
    (* TODO list all patterns *)
    | _ -> raise @@ EvalError "Invalid operands for boolean or"
  end
| And (lhs, rhs) ->
  let rhs_result = eval env rhs in
  let lhs_result = eval env lhs in
  begin
    match rhs_result.res, lhs_result.res with
    | Boolean b1, Boolean b2 -> { res = Boolean (Bool.(&&) b1 b2); new_env = env}
    (* TODO list all patterns *)
    | _, _ -> raise @@ EvalError "Invalid operands for boolean and"
  end
| Comparison _ ->  failwith "TODO"
| Equality _ -> failwith "TODO"
| Assignment (name, expr) ->
  let expr_result = eval env expr in
  let new_env = (name, expr_result.res) :: env in
  {res = expr_result.res; new_env}