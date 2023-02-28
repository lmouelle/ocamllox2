type value =
  | Number of float
  | Nil
  | Boolean of bool
  | String of string
  | Variable of string

type location = Lexing.position

type expr =
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
  | Print of (location * expr)
  | While of (location * expr * expr)

let value_to_string = function
  | Boolean b -> string_of_bool b
  | Variable s -> "$" ^ s
  | String s -> "\"" ^ s ^ "\""
  | Number n -> string_of_float n
  | Nil -> "nil"

let env_to_string env =
  let env_elem_to_string (name, value) =
    "(" ^ name ^ " , " ^ value_to_string value ^ ")"
  in
  List.map env_elem_to_string env |> String.concat " ; "

let rec expr_to_string = function
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
