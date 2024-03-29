open Ocamllox2.Parse_utils
open Ocamllox2.Eval
open Ocamllox2.Ast

(* I don't like this but I wanted some environment where I could
   easily test the _to_string functions I wrote by just entering
   Lox code *)
let rec print_loop _ =
  try
    Printf.printf "> ";
    let ln = read_line () in
    let prog = parse_string ln in
    let results = List.map expr_to_string prog in
    List.iter
      (fun s ->
        print_string s;
        print_newline ())
      results;
    print_loop ()
  with
  (* I hate how the parser error is given the most generic name possible.
     Fully qualify the exception name to make it clear where it comes from *)
  | Ocamllox2.Parser.Error ->
      Printf.printf "Fatal parser error\n";
      print_loop ()
  | End_of_file -> exit 0
  | EvalError (msg, loc) ->
      Printf.printf "Error %s at line %d column %d\n" msg loc.pos_lnum
        loc.pos_cnum;
      print_loop ()

let rec repl env =
  try
    Printf.printf "> ";
    let ln = read_line () in
    let prog = parse_string ln in
    let { res; new_env } = eval_program env prog in
    value_to_string res |> print_string;
    print_newline ();
    repl new_env
  with
  (* I hate how the parser error is given the most generic name possible.
     Fully qualify the exception name to make it clear where it comes from *)
  | Ocamllox2.Parser.Error ->
      Printf.printf "Fatal parser error\n";
      repl env
  | End_of_file -> exit 0
  | EvalError (msg, loc) ->
      Printf.printf "Error %s at line %d column %d\n" msg loc.pos_lnum
        loc.pos_cnum;
      repl env

let () =
  if
    Array.length Sys.argv == 1
    (* No args passed but program name, so repl mode *)
  then (
    print_newline ();
    repl [])
  else
    match Sys.argv.(1) with
    | "-f" | "--filename" ->
        let filename = Sys.argv.(1) in
        let prog = parse_file filename in
        let { res; new_env = _ } = eval_program [] prog in
        value_to_string res |> print_string;
        exit 0
    | "-p" | "--print-interactive" ->
        print_newline ();
        print_loop ()
    | other -> failwith ("Unrecognized interpreter argument " ^ other)
