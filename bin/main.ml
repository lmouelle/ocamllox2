open Ocamllox2.Eval

let rec main env = 
  try
    print_string "> ";
    let ln = read_line () in
    let value = eval ln |> value_to_string in
    print_string value;
    print_newline ();
    main env
  with
  | End_of_file -> exit(0)
  | InvalidExpression msg -> print_string msg; main env

let () = print_newline (); main []
