open Ocamllox2.Parse_utils
open Ocamllox2.Eval

let rec main env =
  try
    Printf.printf "> ";
    let ln = read_line () in
    let prog = parse_string ln in
    let results = List.rev_map (eval env) prog in
    match results with 
    | [] -> main env
    | result :: _ ->
      value_to_string result.res |> print_string;
      main result.new_env
  with
  | End_of_file -> exit 0;
  | EvalError msg -> Printf.printf "%s" msg; main env

let () = print_newline (); main []
