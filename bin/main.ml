open Ocamllox2.Parse_utils
open Ocamllox2.Eval
open Ocamllox2.Ast

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
  | End_of_file -> exit 0
  | EvalError msg ->
      Printf.printf "%s" msg;
      repl env

let () =
  if
    Array.length Sys.argv == 1
    (* No args passed but program name, so repl mode *)
  then (
    print_newline ();
    repl [])
  else
    (* Single arg is script name *)
    let filename = Sys.argv.(1) in
    let prog = parse_file filename in
    let { res; new_env = _ } = eval_program [] prog in
    value_to_string res |> print_string;
    exit 0
