open Parser

exception InvalidExpression of string

let eval s = 
  match parse_with expression_parser s with
  | Error errstr -> raise @@ InvalidExpression errstr
  | Ok _ -> failwith "todo"
