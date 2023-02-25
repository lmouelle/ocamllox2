open Ocamllox2.Parse_utils
open Ocamllox2.Eval
open Ocamllox2.Ast

let rec eval_program env exprs = 
  match exprs with
  | [] -> {res = Nil; new_env = []}
  | single :: [] -> eval env single
  | first :: rest ->
    let {res = _; new_env} = eval env first in
    eval_program new_env rest

let rec main env =
  try
    Printf.printf "> ";
    let ln = read_line () in
    let prog = parse_string ln in
    let {res; new_env} = eval_program env prog in
    value_to_string res |> print_string; print_newline ();
    main new_env

  with
  | End_of_file -> exit 0
  | EvalError msg ->
      Printf.printf "%s" msg;
      main env

let () =
  print_newline ();
  main []
