open Angstrom

(*
  TODO: Handle expressions like (1 - -1) to resolve to 2.
  Currently that just errors out with a parsing error
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


type primary = Number of int | String of string | Boolean of bool | Nil |  Grouping of expression
and expression = BaseExpression of equality
and equality = BaseEquality of comparison | Equals of equality * equality | NotEquals of equality * equality
and comparison = BaseComparison of term | Greater of comparison * comparison | Lesser of comparison * comparison
and term = BaseTerm of factor | Minus of term * term | Plus of term * term
and factor = BaseFactor of unary | Divide of factor * factor | Multiply of factor * factor
and unary = BaseUnary of primary | Negate of unary | Not of unary

type dispatch_table = {
  primary : dispatch_table -> primary t;
  expression : dispatch_table -> expression t;
  equality : dispatch_table -> equality t;
  comparison : dispatch_table -> comparison t;
  term : dispatch_table -> term t;
  factor : dispatch_table -> factor t;
  unary : dispatch_table -> unary t;
}

let primary_parser_inner dt = 
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
  let grouping_parser = 
    whitespace_dropping_parser *>
    (char '(' *> dt.expression dt <* char ')' >>= fun e -> return @@ Grouping e)
    <* whitespace_dropping_parser in
  grouping_parser <|> number_parser <|> boolean_parser <|> nil_parser <|> symbol_parser 

let unary_parser_inner dt = fix (fun unary_parser -> 
  let base_unary_parser = dt.primary dt >>= fun u -> return @@ BaseUnary u in
  let negate_parser = char '-' *> unary_parser >>= fun u -> return @@ Negate u in
  let not_parser = char '!' *> unary_parser >>= fun u -> return @@ Not u in
  base_unary_parser <|> negate_parser <|> not_parser
)

let factor_parser_inner dt =
  let divide_parser = char '/' *> return (fun lhs rhs -> Divide (lhs, rhs)) in
  let multiply_parser = char '*' *> return (fun lhs rhs -> Multiply (lhs, rhs)) in
  let initialvalue = dt.unary dt >>= fun u -> return @@ BaseFactor u in
  chainl1 initialvalue (divide_parser <|> multiply_parser)
let term_parser_inner dt =
  let minus_parser = char '-' *> return (fun lhs rhs -> Minus (lhs, rhs)) in
  let plus_parser = char '+' *> return (fun lhs rhs -> Plus (lhs, rhs)) in
  let initialvalue = dt.factor dt >>= fun f -> return @@ BaseTerm f in
  chainl1 initialvalue (minus_parser <|> plus_parser)
let comparison_parser_inner dt =
  let greater_parser = string ">" *> return (fun lhs rhs -> Greater (lhs, rhs)) in
  let lesser_parser = string "<" *> return (fun lhs rhs -> Lesser (lhs, rhs)) in
  let initialvalue = dt.term dt >>= fun t -> return @@ BaseComparison t in
  chainl1 initialvalue (greater_parser <|> lesser_parser)
let equality_parser_inner dt =
  let equals_parser = string "==" *> return (fun lhs rhs -> Equals (lhs, rhs)) in
  let notequals_parser = string "!=" *> return (fun lhs rhs -> NotEquals (lhs, rhs)) in
  let initialvalue = dt.comparison dt >>= fun c -> return @@ BaseEquality c in
  chainl1 initialvalue (equals_parser <|> notequals_parser)

let expression_parser_inner dt = 
  dt.equality dt >>= fun e -> return @@ BaseExpression e

let dt = {
  primary = primary_parser_inner; 
  expression = expression_parser_inner;
  equality = equality_parser_inner;
  comparison = comparison_parser_inner;
  term = term_parser_inner;
  factor = factor_parser_inner;
  unary = unary_parser_inner
}

let primary_parser = dt.primary dt

let expression_parser = dt.expression dt

let equality_parser = dt.equality dt

let comparison_parser = dt.comparison dt

let term_parser = dt.term dt

let factor_parser = dt.factor dt

let unary_parser = dt.unary dt