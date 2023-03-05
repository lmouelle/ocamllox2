open Ast
open Pretty_print

type eval_result = { value : value; env : env }

exception EvalError of location * string

let rec eval_value loc env = function
  | Variable v when String.empty = v ->
      raise @@ EvalError (loc, "Empty string is not valid for var name")
  | Variable v -> (
      match List.assoc_opt v env with
      | None -> raise @@ EvalError (loc, "No var defined with name " ^ v)
      | Some (Variable v) -> eval_value loc env (Variable v)
      | Some value -> { value; env })
  | _ as value -> { value; env }

let rec eval_numeric_operator loc env lhs rhs f =
  (* TODO: this impl prevents us from using + for string concat. Do we want that? *)
  let lhs_result = eval_expr env lhs in
  let rhs_result = eval_expr env rhs in
  match (lhs_result.value, rhs_result.value) with
  | Number n1, Number n2 -> { value = Number (f n1 n2); env }
  | Nil, _
  | Variable _, _
  | String _, _
  | Boolean _, _
  | _, Nil
  | _, Variable _
  | _, String _
  | _, Boolean _
  | Closure _, _
  | _, Closure _ ->
      raise @@ EvalError (loc, "Invalid operands for numeric binary operation")

and eval_comparison loc env lhs rhs f =
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
             (loc, "Only numeric values are valid operands for comparison")
  in
  let lhs_result = eval_expr env lhs in
  let rhs_result = eval_expr env rhs in
  eval_comparison' lhs_result.value rhs_result.value f

and eval_equality loc env lhs rhs =
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
        raise @@ EvalError (loc, "Cannot compare value to nil")
    | Number _, Boolean _ | Boolean _, Number _ ->
        raise @@ EvalError (loc, "Cannot compare number to bool")
    | String _, Number _ | Number _, String _ ->
        raise @@ EvalError (loc, "Cannot compare number to string")
    | Boolean _, String _ | String _, Boolean _ ->
        raise @@ EvalError (loc, "Cannot compare string to bool")
    | Closure _, _ | _, Closure _ ->
        raise @@ EvalError (loc, "Cannot compare closures for equality")
  in
  let lhs_result = eval_expr env lhs in
  let rhs_result = eval_expr env rhs in
  eval_equality' lhs_result.value rhs_result.value

and eval_invocation loc env evaled_args params body closure_env =
  let calling_env =
    try List.combine params evaled_args
    with Invalid_argument _ ->
      raise
      @@ EvalError
           ( loc,
             "Closure expected args count of "
             ^ (List.length params |> string_of_int) )
  in
  eval_stmt (calling_env @ closure_env @ env) body

and eval_while loc env cond body =
  let rec eval_while' prev_result =
    let cond_result = eval_expr env cond in
    match cond_result.value with
    | Boolean false -> prev_result
    | Boolean true -> eval_while' (eval_stmt env body)
    | _ -> raise @@ EvalError (loc, "While loop must have boolean condition")
  in
  eval_while' { value = Nil; env }

and eval_expr : env -> expr -> eval_result =
 fun env expr ->
  match expr with
  | Value (loc, v) -> eval_value loc env v
  | Or (loc, lhs, rhs) -> (
      let lhs_result = eval_expr env lhs in
      match lhs_result.value with
      | Nil | Variable _ | Number _ | String _ | Closure _ ->
          raise @@ EvalError (loc, "Invalid operands for boolean or")
      | Boolean true -> { value = Boolean true; env }
      | Boolean false -> (
          let rhs_result = eval_expr env rhs in
          match rhs_result.value with
          | Boolean _ as value -> { value; env }
          | Nil | Variable _ | Number _ | String _ | Closure _ ->
              raise @@ EvalError (loc, "Invalid operands for boolean or")))
  | And (loc, lhs, rhs) -> (
      let rhs_result = eval_expr env rhs in
      let lhs_result = eval_expr env lhs in
      match (rhs_result.value, lhs_result.value) with
      | Boolean b1, Boolean b2 -> { value = Boolean (b1 && b2); env }
      | _ -> raise @@ EvalError (loc, "Invalid operands for boolean and"))
  | Plus (loc, lhs, rhs) -> eval_numeric_operator loc env lhs rhs ( +. )
  | Subtract (loc, lhs, rhs) -> eval_numeric_operator loc env lhs rhs ( -. )
  | Multiply (loc, lhs, rhs) -> eval_numeric_operator loc env lhs rhs Float.mul
  | Divide (loc, lhs, rhs) -> eval_numeric_operator loc env lhs rhs ( /. )
  | Equals (loc, lhs, rhs) ->
      { value = Boolean (eval_equality loc env lhs rhs); env }
  | NotEquals (loc, lhs, rhs) ->
      { value = Boolean (not @@ eval_equality loc env lhs rhs); env }
  | Less (loc, lhs, rhs) ->
      { value = Boolean (eval_comparison loc env lhs rhs ( < )); env }
  | LessEqual (loc, lhs, rhs) ->
      { value = Boolean (eval_comparison loc env lhs rhs ( <= )); env }
  | Greater (loc, lhs, rhs) ->
      { value = Boolean (eval_comparison loc env lhs rhs ( > )); env }
  | GreaterEqual (loc, lhs, rhs) ->
      { value = Boolean (eval_comparison loc env lhs rhs ( >= )); env }
  | Not (loc, expr) -> (
      match eval_expr env expr with
      | { value = Boolean b; _ } -> { value = Boolean (not b); env }
      | _ -> raise @@ EvalError (loc, "Not expr requires boolean operand"))
  | Negate (loc, expr) -> (
      match eval_expr env expr with
      | { value = Number n; _ } -> { value = Number (Float.neg n); env }
      | _ -> raise @@ EvalError (loc, "Negate expr requires numeric operand"))
  | Invocation (loc, name, args) -> (
      let evaled_args =
        List.map (eval_expr env) args |> List.map (fun { value; _ } -> value)
      in
      match List.assoc_opt name env with
      | None -> raise @@ EvalError (loc, "No such fun " ^ name)
      | Some (Closure (params, body, closure_env)) ->
          eval_invocation loc env evaled_args params body closure_env
      | Some _ -> raise @@ EvalError (loc, "Cannot invoke non-function"))

and eval_stmt env = function
  | Print (_, expr) ->
      let eval_result = eval_expr env expr in
      let result_str = value_to_string eval_result.value in
      print_string result_str;
      eval_result
  | Expression (_, expr) -> eval_expr env expr
  | Declaration (_, name, None) -> { value = Nil; env = (name, Nil) :: env }
  | Declaration (_, name, Some expr) ->
      let eval_result = eval_expr env expr in
      { eval_result with env = (name, eval_result.value) :: env }
  | Assignment (_, name, expr) ->
      let eval_result = eval_expr env expr in
      { eval_result with env = (name, eval_result.value) :: env }
  | If (loc, cond, iftrue, None) -> (
      let cond_result = eval_expr env cond in
      match cond_result.value with
      | Boolean true -> eval_stmt env iftrue
      | Boolean false -> { value = Nil; env }
      | _ -> raise @@ EvalError (loc, "If stmt must have boolean condition"))
  | If (loc, cond, iftrue, Some iffalse) -> (
      let cond_result = eval_expr env cond in
      match cond_result.value with
      | Boolean true -> eval_stmt env iftrue
      | Boolean false -> eval_stmt env iffalse
      | _ -> raise @@ EvalError (loc, "If stmt must have boolean condition"))
  | Block (_, stmts) -> List.rev_map (eval_stmt env) stmts |> List.hd
  | Function (_, name, params, body) ->
      let value = Closure (params, body, env) in
      { value; env = (name, value) :: env }
  | While (loc, cond, body) -> eval_while loc env cond body

let rec eval_program env stmts =
  match stmts with
  | [] -> { value = Nil; env = [] }
  | [ stmt ] -> eval_stmt env stmt
  | first :: rest ->
      let { env = env'; value = _ } = eval_stmt env first in
      eval_program env' rest
