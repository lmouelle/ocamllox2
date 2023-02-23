open Ocamllox2.Eval

let rec main env = 
  try
    print_string "> ";
    let ln = read_line () in
    let result = eval ln in
    value_to_string result.value |> print_string;
    print_newline ();
    let env' = env @ result.env in
    main env'
  with
  | End_of_file -> exit(0)
  | InvalidExpression msg -> print_string msg; main env

let () = print_newline (); main []
