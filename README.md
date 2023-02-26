# ocamllox2

This was a simple project using the Angstrom parser combinator library to recreate the Lox programming langauge described in the book "Crafting Interpreters"

I've since switched to using a parser generator (Menhir) because while parser combinators are lovely it's harder to parse some things. The string "1 + -1" straight up fails under my Angstrom implementation and requires additional parens.

To setup, just clone this repository. Then do `opam install dune menhir merlin ounit2`. 
You will also need `opam install merlin utop ocaml-lsp-server` to edit this from vscode like I do.

To run the interactive environment you can do `dune exec ./bin/main.exe`.
Even better, install rlwrap using your distro's package manager and just do ./repl.sh within project root