type location = Lexing.position

type value =
  | Number of float
  | Nil
  | Boolean of bool
  | String of string
  | Variable of string
  | Closure of string list * stmt * env

and env = (string * value) list

and expr =
  | Value of location * value
  (* Boolean operators *)
  | Or of location * expr * expr
  | And of location * expr * expr
  (* Binary operators *)
  | Plus of location * expr * expr
  | Subtract of location * expr * expr
  | Divide of location * expr * expr
  | Multiply of location * expr * expr
  (* Equality *)
  | Equals of location * expr * expr
  | NotEquals of location * expr * expr
  (* Binary Comparison *)
  | Less of location * expr * expr
  | LessEqual of location * expr * expr
  | Greater of location * expr * expr
  | GreaterEqual of location * expr * expr
  (* Unary operation *)
  | Not of location * expr
  | Negate of location * expr
  (* Function invocation *)
  | Invocation of location * string * expr list

and stmt =
  | Print of location * expr
  | Expression of location * expr
  | Declaration of location * string * expr option
  | Block of location * stmt list
  | While of location * expr * stmt
  | Function of location * string * string list * stmt
  | If of location * expr * stmt * stmt option
  | Assignment of location * string * expr
