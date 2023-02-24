type _  gadt_value =
| Number : float -> float  gadt_value
| Nil : _ -> _  gadt_value
| Boolean : bool -> bool  gadt_value
| String : string -> string  gadt_value
| Variable : string -> string  gadt_value

type _  gadt_expr =
| Value : 'a  gadt_value -> 'a  gadt_expr
| If : bool  gadt_expr * 'a  gadt_expr * 'a  gadt_expr -> 'a  gadt_expr
| Plus : float  gadt_expr * float  gadt_expr -> float  gadt_expr 
| Subtract : float  gadt_expr * float  gadt_expr -> float  gadt_expr 
| Divide : float  gadt_expr * float  gadt_expr -> float  gadt_expr 
| Multiply : float  gadt_expr * float  gadt_expr -> float  gadt_expr 
| Or : bool  gadt_expr * bool  gadt_expr -> bool  gadt_expr
| And : bool  gadt_expr * bool  gadt_expr -> bool  gadt_expr
(* Instead of seperate rules for equals, lesser, greater, just one to cover them all*)
| Comparison : 'a  gadt_expr * 'a  gadt_expr * ('a -> 'a -> bool) -> bool  gadt_expr

type untyped_value = 
| Number of float
| Nil 
| Boolean of bool
| String of string
| Variable of string

type untyped_expr = 
| Value of untyped_value
| If of untyped_expr * untyped_value * untyped_expr
| Plus of untyped_expr * untyped_expr * untyped_expr 
| Subtract of untyped_expr * untyped_expr * untyped_expr 
| Divide of untyped_expr * untyped_expr * untyped_expr 
| Multiply of untyped_expr * untyped_expr * untyped_expr 
| Or of untyped_expr * untyped_expr * untyped_expr
| And  of untyped_expr * untyped_expr * untyped_expr
(* Instead of seperate rules for equals, lesser, greater, just one to cover them all*)
| Comparison : untyped_expr * untyped_expr * ('a -> 'a -> bool) -> untyped_expr