open Ast

let rec value_to_string = function
  | Boolean b -> string_of_bool b
  | Variable s -> "$" ^ s
  | String s -> "\"" ^ s ^ "\""
  | Number n -> string_of_float n
  | Nil -> "nil"
  | Closure (params, body, _) ->
      (* TODO: Do we want to print env with closures? Seems really verbose... *)
      "fun" ^ "(" ^ String.concat "," params ^ ")" ^ "{" ^ stmt_to_string body
      ^ "}"

and env_to_string env =
  let env_elem_to_string (name, value) =
    "(" ^ name ^ " , " ^ value_to_string value ^ ")"
  in
  List.map env_elem_to_string env |> String.concat " ; "

and expr_to_string : expr -> string = function
  | Value (_, v) -> value_to_string v
  | Or (_, lhs, rhs) ->
      "Or(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | And (_, lhs, rhs) ->
      "And(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | Plus (_, lhs, rhs) ->
      "+(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | Subtract (_, lhs, rhs) ->
      "-(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | Divide (_, lhs, rhs) ->
      "/(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | Multiply (_, lhs, rhs) ->
      "*(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | Equals (_, lhs, rhs) ->
      "==(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | NotEquals (_, lhs, rhs) ->
      "!=(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | Not (_, expr) -> "!(" ^ expr_to_string expr ^ ")"
  | Less (_, lhs, rhs) ->
      ">(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | LessEqual (_, lhs, rhs) ->
      ">=(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | Greater (_, lhs, rhs) ->
      "<(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | GreaterEqual (_, lhs, rhs) ->
      "<=(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | Invocation (_, v, args) ->
      "Invoke(" ^ v ^ ","
      ^ (List.map expr_to_string args |> String.concat ",")
      ^ ")"
  | Negate (_, e) -> "-(" ^ expr_to_string e ^ ")"

and stmt_to_string = function
  | Print (_, e) -> "print(" ^ expr_to_string e ^ ")"
  | Expression (_, e) -> "(" ^ expr_to_string e ^ ")"
  | Declaration (_, name, e) ->
      let e_str = match e with None -> "" | Some e -> expr_to_string e in
      "decl(" ^ name ^ "," ^ e_str ^ ")"
  | Block (_, b) -> "{" ^ (List.map stmt_to_string b |> String.concat ";") ^ "}"
  | While (_, cond, body) ->
      "while(" ^ expr_to_string cond ^ "," ^ stmt_to_string body ^ ")"
  | Function (_, name, params, body) ->
      "fun(" ^ name ^ "," ^ String.concat "," params ^ ")" ^ "{"
      ^ stmt_to_string body ^ "}"
  | If (_, cond, iftrue, iffalse) ->
      let iffalse_str =
        match iffalse with None -> "" | Some stmt -> stmt_to_string stmt
      in
      "if(" ^ expr_to_string cond ^ "," ^ stmt_to_string iftrue ^ ","
      ^ iffalse_str ^ ")"
  | Assignment (_, name, e) -> "=(" ^ name ^ "," ^ expr_to_string e ^ ")"

let program_to_string prog =
  List.map stmt_to_string prog |> String.concat ","