open Ast

exception EvalError of string * location

type eval_result = { res : value; new_env : (string * value) list }

let value_to_string = function
  | Boolean b -> string_of_bool b
  | Variable s -> "$" ^ s
  | String s -> "\"" ^ s ^ "\""
  | Number n -> string_of_float n
  | Nil -> "nil"

let rec eval (env : (string * value) list) (expr : expr) =
  let eval_numeric_operator loc lhs rhs f =
    (* TODO: this impl prevents us from using + for string concat. Do we want that? *)
    let lhs_result = eval env lhs in
    let rhs_result = eval env rhs in
    match (lhs_result.res, rhs_result.res) with
    | Number n1, Number n2 -> { res = Number (f n1 n2); new_env = env }
    | Nil, _
    | Variable _, _
    | String _, _
    | Boolean _, _
    | _, Nil
    | _, Variable _
    | _, String _
    | _, Boolean _ ->
        raise @@ EvalError ("Invalid operands for numeric binary operation", loc)
  in
  let eval_comparison loc lhs rhs f =
    let rec eval_comparison' lhs_value rhs_value f =
      match (lhs_value, rhs_value) with
      | Number n, Number n1 -> f n n1
      | Variable v, Number n | Number n, Variable v ->
          eval_comparison' (List.assoc v env) (Number n) f
      | Variable v1, Variable v2 ->
          eval_comparison' (List.assoc v1 env) (List.assoc v2 env) f
      | _ ->
          raise
          @@ EvalError
               ("Only numeric values are valid operands for comparison", loc)
    in
    let lhs_result = eval env lhs in
    let rhs_result = eval env rhs in
    eval_comparison' lhs_result.res rhs_result.res f
  in
  let eval_equality loc lhs rhs =
    let rec eval_equality' lhs_value rhs_value =
      match (lhs_value, rhs_value) with
      | Number n1, Number n2 -> n1 = n2
      | Boolean n1, Boolean n2 -> n1 = n2
      | String n1, String n2 -> n1 = n2
      | Nil, Nil -> true
      | Variable v1, Variable v2 ->
          eval_equality' (List.assoc v1 env) (List.assoc v2 env)
      | (_ as value), Variable v -> eval_equality' (List.assoc v env) value
      | Variable v, Number n -> eval_equality' (List.assoc v env) (Number n)
      | Variable v, String s -> eval_equality' (List.assoc v env) (String s)
      | Variable v, Nil -> eval_equality' (List.assoc v env) Nil
      | Variable v, Boolean b -> eval_equality' (List.assoc v env) (Boolean b)
      | Nil, Number _
      | Nil, Boolean _
      | Nil, String _
      | Number _, Nil
      | Boolean _, Nil
      | String _, Nil ->
          raise @@ EvalError ("Cannot compare value to nil", loc)
      | Number _, Boolean _ | Boolean _, Number _ ->
          raise @@ EvalError ("Cannot compare number to bool", loc)
      | String _, Number _ | Number _, String _ ->
          raise @@ EvalError ("Cannot compare number to string", loc)
      | Boolean _, String _ | String _, Boolean _ ->
          raise @@ EvalError ("Cannot compare string to bool", loc)
    in
    let lhs_result = eval env lhs in
    let rhs_result = eval env rhs in
    eval_equality' lhs_result.res rhs_result.res
  in
  let rec eval_value loc = function
    | Number n -> { res = Number n; new_env = env }
    | Boolean b -> { res = Boolean b; new_env = env }
    | Nil -> { res = Nil; new_env = env }
    | String s -> { res = String s; new_env = env }
    | Variable v when String.empty = v ->
        raise @@ EvalError ("Empty string is not valid for var name", loc)
    | Variable v -> (
        match List.assoc_opt v env with
        | None -> raise @@ EvalError ("No var defined with name " ^ v, loc)
        | Some (Variable v) -> eval_value loc (Variable v)
        | Some value -> { res = value; new_env = env })
  in

  match expr with
  | Value (loc, v) -> eval_value loc v
  | If (loc, cond, iftrue, iffalse) -> (
      let result = eval env cond in
      match result.res with
      | Boolean true -> eval env iftrue
      | Boolean false -> eval env iffalse
      | Number _ | String _ | Nil | Variable _ ->
          raise @@ EvalError ("Invalid condition type for if expr", loc))
  | Plus (loc, lhs, rhs) -> eval_numeric_operator loc lhs rhs Float.add
  | Multiply (loc, lhs, rhs) -> eval_numeric_operator loc lhs rhs Float.mul
  | Subtract (loc, lhs, rhs) -> eval_numeric_operator loc lhs rhs Float.sub
  | Divide (loc, lhs, rhs) -> eval_numeric_operator loc lhs rhs Float.div
  | Or (loc, lhs, rhs) -> (
      let lhs_result = eval env lhs in
      match lhs_result.res with
      | Nil | Variable _ | Number _ | String _ ->
          raise @@ EvalError ("Invalid operands for boolean or", loc)
      | Boolean true -> { res = Boolean true; new_env = env }
      | Boolean false -> (
          let rhs_result = eval env rhs in
          match rhs_result.res with
          | Boolean true -> { res = Boolean true; new_env = env }
          | Boolean false -> { res = Boolean false; new_env = env }
          | Nil | Variable _ | Number _ | String _ ->
              raise @@ EvalError ("Invalid operands for boolean or", loc)))
  | And (loc, lhs, rhs) -> (
      let rhs_result = eval env rhs in
      let lhs_result = eval env lhs in
      match (rhs_result.res, lhs_result.res) with
      | Boolean b1, Boolean b2 ->
          { res = Boolean (Bool.( && ) b1 b2); new_env = env }
      | _ -> raise @@ EvalError ("Invalid operands for boolean and", loc))
  | Assignment (_, name, expr) ->
      (* TODO: Should I support variable shadowing or error out if I try to define the same var twice? *)
      let expr_result = eval env expr in
      let new_env = (name, expr_result.res) :: env in
      { res = expr_result.res; new_env }
  | Equals (loc, lhs, rhs) ->
      { res = Boolean (eval_equality loc lhs rhs); new_env = env }
  | NotEquals (loc, lhs, rhs) ->
      { res = Boolean (not @@ eval_equality loc lhs rhs); new_env = env }
  | Less (loc, lhs, rhs) ->
      { res = Boolean (eval_comparison loc lhs rhs ( < )); new_env = env }
  | LessEqual (loc, lhs, rhs) ->
      { res = Boolean (eval_comparison loc lhs rhs ( <= )); new_env = env }
  | Greater (loc, lhs, rhs) ->
      { res = Boolean (eval_comparison loc lhs rhs ( > )); new_env = env }
  | GreaterEqual (loc, lhs, rhs) ->
      { res = Boolean (eval_comparison loc lhs rhs ( >= )); new_env = env }
  | Grouping (_, expr) -> eval env expr
  | Not (loc, expr) -> (
      let eval_result = eval env expr in
      match eval_result.res with
      | Boolean b -> { eval_result with res = Boolean (not b) }
      | Number _ | Nil | String _ | Variable _ ->
          raise @@ EvalError ("Not operator must have boolean operand", loc))
  | Print(_, expr) ->
    let eval_result = eval env expr in
    value_to_string eval_result.res |> print_string;
    eval_result

let rec eval_program env exprs =
  match exprs with
  | [] -> { res = Nil; new_env = [] }
  | single :: [] -> eval env single
  | first :: rest ->
      let { res = _; new_env } = eval env first in
      eval_program new_env rest
