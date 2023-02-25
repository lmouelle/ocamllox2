type value = 
| Number of float
| Nil 
| Boolean of bool
| String of string
| Variable of string

type expr = 
| Value of value
| If of expr * expr * expr
| Plus of expr * expr 
| Subtract of expr * expr 
| Divide of expr * expr 
| Multiply of expr * expr 
| Or of expr * expr
| And  of expr * expr
(* Instead of seperate rules for lesser, greater, etc just one to cover them all*)
| Comparison : expr * expr * ('a -> 'a -> bool) -> expr
| Assignment of string * expr
(* Need equality seperate from comparison because equality works on both nums and bools, but comparison only on nums*)
| Equality : expr * expr * ('a -> 'a -> bool) -> expr