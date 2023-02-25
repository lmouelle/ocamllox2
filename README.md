# ocamllox2

This was a simple project using the Angstrom parser combinator library to recreate the Lox programming langauge described in the book "Crafting Interpreters"

I've since switched to using a parser generator (Menhir) because while parser combinators are lovely it's harder to parse some things. The string "1 + -1" straight up fails under my Angstrom implementation and requires additional parens.
