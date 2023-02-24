open Ast

exception EvalError of string

type eval_result = {
  res : value; 
  new_env : (string * value) list;
}

let rec eval = function
| Value v -> {res = v; new_env = []}
| If(cond, iftrue, iffalse) ->
  begin
    let result = eval cond in
    match result.res with 
    | Boolean true -> eval iftrue
    | Boolean false -> eval iffalse
    | Number _ | String _ | Nil | Variable _ -> raise @@ EvalError "Invalid condition type for if expr"
  end
| Plus (lhs, rhs) ->
  let lhs_result = eval lhs in
  let rhs_result = eval rhs in
  begin
    match lhs_result.res, rhs_result.res with
    | Number n1, Number n2 -> {res = Number (Float.add n1 n2); new_env = []}
    | _, _ -> (* TODO  instead of wildcard case match list all possible matches for better type safety *)
    raise @@ EvalError "Invalid operands for addition"
  end
| Multiply (lhs, rhs) ->
let lhs_result = eval lhs in
let rhs_result = eval rhs in
begin
match lhs_result.res, rhs_result.res with
| Number n1, Number n2 -> {res = Number (Float.mul n1 n2); new_env = []}
| _, _ -> (* TODO  instead of wildcard case match list all possible matches for better type safety *)
raise @@ EvalError "Invalid operands for multiplication"
end
| Subtract (lhs, rhs) ->
let lhs_result = eval lhs in
let rhs_result = eval rhs in
begin
  match lhs_result.res, rhs_result.res with
  | Number n1, Number n2 -> {res = Number (Float.sub n1 n2); new_env = []}
  | _, _ -> (* TODO  instead of wildcard case match list all possible matches for better type safety *)
  raise @@ EvalError "Invalid operands for subtraction"
end    
| Divide (lhs, rhs) ->
  let lhs_result = eval lhs in
  let rhs_result = eval rhs in
  begin
    match lhs_result.res, rhs_result.res with
    | Number n1, Number n2 -> {res = Number (Float.div n1 n2); new_env = []}
    | _, _ -> (* TODO  instead of wildcard case match list all possible matches for better type safety *)
    raise @@ EvalError "Invalid operands for division"
  end      
| Or (lhs, rhs) ->
  let lhs_result = eval lhs in
  begin 
    match lhs_result.res with
    | Boolean true -> { res = Boolean true; new_env = [] }
    | Boolean false -> eval rhs
    (* TODO list all patterns *)
    | _ -> raise @@ EvalError "Invalid operands for boolean or"
  end
| And (lhs, rhs) ->
  let rhs_result = eval rhs in
  let lhs_result = eval lhs in
  begin
    match rhs_result.res, lhs_result.res with
    | Boolean b1, Boolean b2 -> { res = Boolean (Bool.(&&) b1 b2); new_env = []}
    (* TODO list all patterns *)
    | _, _ -> raise @@ EvalError "Invalid operands for boolean and"
  end
| Comparison _ -> failwith "TODO"