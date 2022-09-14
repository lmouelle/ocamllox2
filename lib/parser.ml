open Angstrom

(* 
  TODO: Drop intermediate whitespace 
  Handle paranthesized expressions like (1 + 1)   
*)

type expression = Literal of literal | Unary of unary | Binary of binary | Grouping of expression
and literal = Number of int | String of string | Boolean of bool
and unary = Negate of expression | Not of expression
and binary = {op : operator; lhs : expression; rhs : expression}
and operator = Equals | NotEquals | Greater | Lesser | Plus | Minus | Divide | Multiply

let number = take_while1 (function '0' .. '9' -> true | _ -> false) >>= fun s -> return @@ Number (int_of_string s)
let string = take_while1 (function ')' | '(' | ' ' | '\t' | ';' -> false | _ -> true) >>= fun s -> return @@ String s
let boolean = (Angstrom.string "true" *> return (Boolean true)) <|> (Angstrom.string "false" *> return (Boolean false))


let is_seperating_whitespace = function ' ' | '\t' -> true | _ -> false

let whitespace_dropping_parser = skip_while is_seperating_whitespace

let literal = number <|> boolean <|> string

let operator =
  (Angstrom.string "==" *> return Equals) <|>
  (Angstrom.string "!=" *> return NotEquals) <|>
  (Angstrom.string ">" *> return Greater) <|>
  (Angstrom.string "<" *> return Lesser) <|>
  (Angstrom.string "+" *> return Plus) <|>
  (Angstrom.string "-" *> return Minus) <|>
  (Angstrom.string "/" *> return Divide) <|>
  (Angstrom.string "*" *> return Multiply)

let expression = fix (fun expr : expression t -> 
  let grouping = char '(' *> expr <* char ')' in
  let unary = (Angstrom.string "!" *> expr >>= fun x -> return @@ Not x) <|> (Angstrom.string "-" *> expr >>= fun x -> return @@ Negate x) in
  let binary = expr >>= fun lhs -> operator >>= fun op -> expr >>= fun rhs -> return {op; lhs; rhs} in
  (grouping >>= fun g -> return @@ Grouping g) <|>
  (unary >>= fun u -> return @@ Unary u) <|>
  (binary >>= fun b -> return @@ Binary b) <|> 
  (literal >>= fun lit -> return @@ Literal lit)
)

let parse_with str parser = parse_string ~consume:All parser str

let parse s = parse_with s expression
