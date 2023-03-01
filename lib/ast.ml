type location = Lexing.position

type value =
  | Number of float
  | Nil
  | Boolean of bool
  | String of string
  | Variable of string
  | Closure of string list * expr * env

and env = (string * value) list

and expr =
  | Value of location * value
  | If of location * expr * expr * expr
  | Or of location * expr * expr
  | And of location * expr * expr
  | Assignment of location * string * expr
  | Mutation of location * string * expr
  (* Binary operators *)
  | Plus of location * expr * expr
  | Subtract of location * expr * expr
  | Divide of location * expr * expr
  | Multiply of location * expr * expr
  (* Equality *)
  | Equals of location * expr * expr
  | NotEquals of location * expr * expr
  (* Comparison *)
  | Less of location * expr * expr
  | LessEqual of location * expr * expr
  | Greater of location * expr * expr
  | GreaterEqual of location * expr * expr
  | Grouping of location * expr
  | Not of location * expr
  | Print of location * expr
  | While of location * expr * expr
  | Invocation of location * string * expr list
  (* TODO: I don't like this. I do not like having both closure, the value,
     and Function (which is basically the same as closure) but as an expression.
     However I cannot just create a Closure from within the parser.mly. Why?
     Because the parser has no idea of an environment and Closure needs an environment.
     So in the parser we parse out a Function, and then within eval we construct the environment.
     Then we construct the Closure within eval and add it to the environment. *)
  | Function of location * string list * expr

let rec value_to_string = function
  | Boolean b -> string_of_bool b
  | Variable s -> "$" ^ s
  | String s -> "\"" ^ s ^ "\""
  | Number n -> string_of_float n
  | Nil -> "nil"
  | Closure (params, body, _) ->
      (* TODO: Do we want to print env with closures? Seems really verbose... *)
      "fun" ^ "(" ^ String.concat "," params ^ ")" ^ "{" ^ expr_to_string body
      ^ "}"

and env_to_string env =
  let env_elem_to_string (name, value) =
    "(" ^ name ^ " , " ^ value_to_string value ^ ")"
  in
  List.map env_elem_to_string env |> String.concat " ; "

and expr_to_string = function
  | Value (_, v) -> value_to_string v
  | If (_, cond, iftrue, iffalse) ->
      "If(" ^ expr_to_string cond ^ "," ^ expr_to_string iftrue ^ ","
      ^ expr_to_string iffalse ^ ")"
  | Or (_, lhs, rhs) ->
      "Or(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | And (_, lhs, rhs) ->
      "And(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | Assignment (_, name, expr) ->
      "Assignment(" ^ name ^ "=" ^ expr_to_string expr ^ ")"
  | Mutation (_, name, expr) ->
      "Mutation(" ^ name ^ "=" ^ expr_to_string expr ^ ")"
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
  | Grouping (_, expr) -> "(" ^ expr_to_string expr ^ ")"
  | Not (_, expr) -> "!(" ^ expr_to_string expr ^ ")"
  | Print (_, expr) -> "print(" ^ expr_to_string expr ^ ")"
  | Less (_, lhs, rhs) ->
      ">(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | LessEqual (_, lhs, rhs) ->
      ">=(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | Greater (_, lhs, rhs) ->
      "<(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | GreaterEqual (_, lhs, rhs) ->
      "<=(" ^ expr_to_string lhs ^ "," ^ expr_to_string rhs ^ ")"
  | While (_, cond, body) ->
      "while(" ^ expr_to_string cond ^ "," ^ expr_to_string body ^ ")"
  | Invocation (_, name, args) ->
      "Invoke(" ^ name ^ ","
      ^ (List.map expr_to_string args |> String.concat ",")
      ^ ")"
  | Function (_, params, body) ->
      "fun(" ^ String.concat "," params ^ "," ^ expr_to_string body ^ ")"
