open Parser

exception Invalidastession of string

type value =
| NumberValue of int
| BoolValue of bool
| SymbolValue of string
| NilValue

type ast =
| ValueAst of value
| EqualsAst of ast * ast
| NotEqualsAst of ast * ast
| PlusAst of ast * ast
| MinusAst of ast * ast
| DivideAst of ast * ast
| MultiplyAst of ast * ast
| GreaterAst of ast * ast
| LesserAst of ast * ast
| NotAst of ast
| NegateAst of ast

let rec transform_primary = function
| Boolean b -> ValueAst (BoolValue b)
| Number i -> ValueAst (NumberValue i)
| String s -> ValueAst (SymbolValue s)
| Nil -> ValueAst (NilValue)
| Grouping e -> transform_expression e

and transform_expression = function
| BaseExpression e -> transform_equality e

and transform_equality = function
| BaseEquality comparison -> transform_comparison comparison
| Equals (lhs, rhs) ->
  let lhs' = transform_equality lhs in
  let rhs' = transform_equality rhs in
  EqualsAst (lhs', rhs')
| NotEquals (lhs, rhs) ->
  let lhs' = transform_equality lhs in
  let rhs' = transform_equality rhs in
  NotEqualsAst (lhs', rhs')

and transform_comparison = function
| BaseComparison term -> transform_term term
| Greater (lhs, rhs) ->
  let lhs' = transform_comparison lhs in
  let rhs' = transform_comparison rhs in
  GreaterAst (lhs', rhs')
| Lesser (lhs, rhs) ->
  let lhs' = transform_comparison lhs in
  let rhs' = transform_comparison rhs in
  LesserAst (lhs', rhs')

and transform_term = function
| BaseTerm factor -> transform_factor factor
| Minus (lhs, rhs) ->
  let lhs' = transform_term lhs in
  let rhs' = transform_term rhs in
  MinusAst (lhs', rhs')
| Plus (lhs, rhs) ->
  let lhs' = transform_term lhs in
  let rhs' = transform_term rhs in
  PlusAst (lhs', rhs')

and transform_factor = function
| BaseFactor unary -> transform_unary unary
| Divide (lhs, rhs) -> DivideAst (transform_factor lhs, transform_factor rhs)
| Multiply (lhs, rhs) -> MultiplyAst (transform_factor lhs, transform_factor rhs)

and transform_unary = function
| BaseUnary primary -> transform_primary primary
| Not unary -> NotAst (transform_unary unary)
| Negate unary -> NegateAst (transform_unary unary)