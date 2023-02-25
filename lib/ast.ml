type value =
  | Number of float
  | Nil
  | Boolean of bool
  | String of string
  | Variable of string

type expr =
  | Value of value
  | If of expr * expr * expr
  | Or of expr * expr
  | And of expr * expr
  | Assignment of string * expr
  (* Binary operators *)
  | Plus of expr * expr
  | Subtract of expr * expr
  | Divide of expr * expr
  | Multiply of expr * expr
  (* Equality *)
  | Equals of expr * expr
  | NotEquals of expr * expr
  (* Comparison *)
  | Less of expr * expr
  | LessEqual of expr * expr
  | Greater of expr * expr
  | GreaterEqual of expr * expr
