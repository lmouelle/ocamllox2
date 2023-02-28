# ocamllox2

This was a simple project using the Angstrom parser combinator library to recreate the Lox programming langauge described in the book "Crafting Interpreters"

I've since switched to using a parser generator (Menhir) because while parser combinators are lovely it's harder to parse some things. The string "1 + -1" straight up fails under my Angstrom implementation and requires additional parens.

To setup, just clone this repository. Then do `opam install dune menhir ounit2`. 
You will also need `opam install merlin utop ocaml-lsp-server` to edit this from vscode like I do.

To run the interactive environment you can do `dune exec ./bin/main.exe`.
Even better, install rlwrap using your distro's package manager and just do ./repl.sh within project root

# Where I diverge from the Lox language defined in Crafting Interpreters

I don't like all the things in that book. Here are a few changes that I made:
- While loop syntax and If expression use (condition) {body/iftrue} {iffalse}.
  Currently there is no way to make an if expression without an else block. 
  Becuase I want it to remain an expression after all.
- No statements. Everything in my language is an expression of some kind.
  Loops eval to the last value of their body, assignment and declaration eval to themselves
- No for loop. Yet. I don't want something like C-style indexed for-loops but closer to Python for/Java foreach
  over an iterator. But that is wayyyy further down the line
- Right now I allow some wacky things. You can declare a variable pretty much anywhere.
  It's all lexically scoped so if you declare a variable like this:
  ```
  if (var foo = 0; true) { print true } { print false }
  ```
  then var foo is effectively ignored. It won't be added to the environment anywhere.
  This is really confusing and changing it is on my TODO list though I consider it a lower priority