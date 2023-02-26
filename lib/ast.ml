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
