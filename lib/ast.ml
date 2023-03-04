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
  | Assignment of location * string * expr

type stmt =
  | Print of location * expr
  | Expression of location * expr
  | Declaration of location * string * expr option
  | Block of location * stmt list
  | While of location * expr * stmt
  | Function of location * string * string list * stmt
  | If of location * expr * stmt * stmt option

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
  | Assignment (_, name, e) -> "=(" ^ name ^ "," ^ expr_to_string e ^ ")"

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
