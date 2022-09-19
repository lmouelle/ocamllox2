open Angstrom

(* 
  TODO: Handle paranthesized expressions like (1 + 1)   
*)

let chainl1 e op =
  let rec go acc =
    (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
  e >>= go

let rec chainr1 e op = 
  e >>= fun (result) -> (op >>= fun f -> chainr1 e op >>| f result) <|> return result

let parse_with parser str = parse_string ~consume:All parser str

let is_seperating_whitespace = function ' ' | '\t' -> true | _ -> false
let whitespace_dropping_parser = skip_while is_seperating_whitespace


type primary = Number of int | String of string | Boolean of bool | Nil | Grouping of expression
and expression = BaseExpression of equality
and equality = BaseEquality of comparison | Equals of equality * equality | NotEquals of equality * equality
and comparison = BaseComparison of term | Greater of comparison * comparison | Lesser of comparison * comparison
and term = BaseTerm of factor | Minus of term * term | Plus of term * term
and factor = BaseFactor of unary | Divide of factor * factor | Multiply of factor * factor
and unary = BaseUnary of primary | Negate of unary | Not of unary

let primary_parser = 
  let is_number_char = function '0' .. '9' -> true | _ -> false in
  let is_string_char = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
  let number_parser = 
    whitespace_dropping_parser *> 
    (take_while1 is_number_char >>| int_of_string >>= fun i -> return @@ Number i)
    <* whitespace_dropping_parser in
  let symbol_parser =
    whitespace_dropping_parser *> 
    (take_while1 is_string_char >>= fun s -> return @@ String s) 
    <* whitespace_dropping_parser in
  let boolean_parser = 
    whitespace_dropping_parser *>
    (Angstrom.string "true" *> (return @@ Boolean true)  <|> Angstrom.string "false" *> (return @@ Boolean false))
    <* whitespace_dropping_parser in
  let nil_parser = 
    whitespace_dropping_parser *>
    (Angstrom.string "nil" *> return Nil)
    <* whitespace_dropping_parser in
  number_parser <|> boolean_parser <|> nil_parser <|> symbol_parser 

let unary_parser = fix (fun unary_parser -> 
  let base_unary_parser = primary_parser >>= fun u -> return @@ BaseUnary u in
  let negate_parser = char '-' *> unary_parser >>= fun u -> return @@ Negate u in
  let not_parser = char '!' *> unary_parser >>= fun u -> return @@ Not u in
  base_unary_parser <|> negate_parser <|> not_parser
)

let factor_parser =
  let divide_parser = char '/' *> return (fun lhs rhs -> Divide (lhs, rhs)) in
  let multiply_parser = char '*' *> return (fun lhs rhs -> Multiply (lhs, rhs)) in
  let initialvalue = unary_parser >>= fun u -> return @@ BaseFactor u in
  chainl1 initialvalue (divide_parser <|> multiply_parser)
let term_parser =
  let minus_parser = char '-' *> return (fun lhs rhs -> Minus (lhs, rhs)) in
  let plus_parser = char '+' *> return (fun lhs rhs -> Plus (lhs, rhs)) in
  let initialvalue = factor_parser >>= fun f -> return @@ BaseTerm f in
  chainl1 initialvalue (minus_parser <|> plus_parser)
let comparison_parser =
  let greater_parser = string ">" *> return (fun lhs rhs -> Greater (lhs, rhs)) in
  let lesser_parser = string "<" *> return (fun lhs rhs -> Lesser (lhs, rhs)) in
  let initialvalue = term_parser >>= fun t -> return @@ BaseComparison t in
  chainl1 initialvalue (greater_parser <|> lesser_parser)
let equality_parser =
  let equals_parser = string "==" *> return (fun lhs rhs -> Equals (lhs, rhs)) in
  let notequals_parser = string "!=" *> return (fun lhs rhs -> NotEquals (lhs, rhs)) in
  let initialvalue = comparison_parser >>= fun c -> return @@ BaseEquality c in
  chainl1 initialvalue (equals_parser <|> notequals_parser)

let expression_parser = 
  equality_parser >>= fun e -> return @@ BaseExpression e