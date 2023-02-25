open Ast

exception EvalError of string

type eval_result = { res : value; new_env : (string * value) list }

let value_to_string = function
  | Boolean b -> string_of_bool b
  | Variable s | String s -> s
  | Number n -> string_of_float n
  | Nil -> "nil"

let rec eval (env : (string * value) list) (expr : expr) =
  let number_binary_op_check lhs rhs f =
    (* Note: this impl prevents us from using + for string concat. Do we want that? *)
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
        raise @@ EvalError "Invalid operands for numeric binary operation"
  in
  let comparison_check lhs rhs f =
    let rec comparison_check' lhs_value rhs_value f =
      match (lhs_value, rhs_value) with
      | Number n, Number n1 -> f n n1
      | Variable v, Number n | Number n, Variable v ->
          comparison_check' (List.assoc v env) (Number n) f
      | Variable v1, Variable v2 ->
          comparison_check' (List.assoc v1 env) (List.assoc v2 env) f
      | _ ->
          raise
          @@ EvalError
               "Invalid operands for comparison check, only numeric values are \
                valid"
    in
    let lhs_result = eval env lhs in
    let rhs_result = eval env rhs in
    comparison_check' lhs_result.res rhs_result.res f
  in
  let equality_check lhs rhs =
    let rec equality_check' lhs_value rhs_value =
      match (lhs_value, rhs_value) with
      | Number n1, Number n2 -> n1 = n2
      | Boolean n1, Boolean n2 -> n1 = n2
      | String n1, String n2 -> n1 = n2
      | Nil, Nil -> true
      | Variable v1, Variable v2 ->
          equality_check' (List.assoc v1 env) (List.assoc v2 env)
      | (_ as value), Variable v -> equality_check' (List.assoc v env) value
      | Variable v, Number n -> equality_check' (List.assoc v env) (Number n)
      | Variable v, String s -> equality_check' (List.assoc v env) (String s)
      | Variable v, Nil -> equality_check' (List.assoc v env) Nil
      | Variable v, Boolean b -> equality_check' (List.assoc v env) (Boolean b)
      | Nil, Number _
      | Nil, Boolean _
      | Nil, String _
      | Number _, Nil
      | Boolean _, Nil
      | String _, Nil ->
          raise @@ EvalError "Cannot compare value to nil"
      | Number _, Boolean _ | Boolean _, Number _ ->
          raise @@ EvalError "Cannot compare number to bool"
      | String _, Number _ | Number _, String _ ->
          raise @@ EvalError "Cannot compare number to string"
      | Boolean _, String _ | String _, Boolean _ ->
          raise @@ EvalError "Cannot compare string to bool"
    in
    let lhs_result = eval env lhs in
    let rhs_result = eval env rhs in
    equality_check' lhs_result.res rhs_result.res
  in
  match expr with
  | Value v -> { res = v; new_env = env }
  | If (cond, iftrue, iffalse) -> (
      let result = eval env cond in
      match result.res with
      | Boolean true -> eval env iftrue
      | Boolean false -> eval env iffalse
      | Number _ | String _ | Nil | Variable _ ->
          raise @@ EvalError "Invalid condition type for if expr")
  | Plus (lhs, rhs) -> number_binary_op_check lhs rhs Float.add
  | Multiply (lhs, rhs) -> number_binary_op_check lhs rhs Float.mul
  | Subtract (lhs, rhs) -> number_binary_op_check lhs rhs Float.sub
  | Divide (lhs, rhs) -> number_binary_op_check lhs rhs Float.div
  | Or (lhs, rhs) -> (
      let lhs_result = eval env lhs in
      match lhs_result.res with
      | Nil | Variable _ | Number _ | String _ ->
          raise @@ EvalError "Invalid operands for boolean or"
      | Boolean true -> { res = Boolean true; new_env = env }
      | Boolean false -> (
          let rhs_result = eval env rhs in
          match rhs_result.res with
          | Boolean true -> { res = Boolean true; new_env = env }
          | Boolean false -> { res = Boolean false; new_env = env }
          | Nil | Variable _ | Number _ | String _ ->
              raise @@ EvalError "Invalid operands for boolean or"))
  | And (lhs, rhs) -> (
      let rhs_result = eval env rhs in
      let lhs_result = eval env lhs in
      match (rhs_result.res, lhs_result.res) with
      | Boolean b1, Boolean b2 ->
          { res = Boolean (Bool.( && ) b1 b2); new_env = env }
      | _ -> raise @@ EvalError "Invalid operands for boolean and")
  | Assignment (name, expr) ->
      let expr_result = eval env expr in
      let new_env = (name, expr_result.res) :: env in
      { res = expr_result.res; new_env }
  | Equals (lhs, rhs) ->
      { res = Boolean (equality_check lhs rhs); new_env = env }
  | NotEquals (lhs, rhs) ->
      { res = Boolean (not @@ equality_check lhs rhs); new_env = env }
  | Less (lhs, rhs) ->
      { res = Boolean (comparison_check lhs rhs ( < )); new_env = env }
  | LessEqual (lhs, rhs) ->
      { res = Boolean (comparison_check lhs rhs ( <= )); new_env = env }
  | Greater (lhs, rhs) ->
      { res = Boolean (comparison_check lhs rhs ( > )); new_env = env }
  | GreaterEqual (lhs, rhs) ->
      { res = Boolean (comparison_check lhs rhs ( >= )); new_env = env }

let rec eval_program env exprs =
  match exprs with
  | [] -> { res = Nil; new_env = [] }
  | single :: [] -> eval env single
  | first :: rest ->
      let { res = _; new_env } = eval env first in
      eval_program new_env rest
