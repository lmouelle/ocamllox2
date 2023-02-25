open Ocamllox2.Parse_utils
open Ocamllox2.Eval

let rec main env =
  try
    Printf.printf "> ";
    let ln = read_line () in
    let prog = parse_string ln in
    let result = eval prog in
    value_to_string result.res |> print_string;
    main result.new_env

  with
  | End_of_file -> exit 0;
  | EvalError msg -> Printf.printf "%s" msg; main env

let () = print_newline (); main []
