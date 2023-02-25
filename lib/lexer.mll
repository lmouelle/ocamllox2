{
    open Lexing
    open Parser
    exception SyntaxError of string

    let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let newline = '\r' | '\n' | "\r\n"
let whitespace = [' ' '\t']+
let digit = '-'? ['0'-'9']+ 
let identifier = ['a'-'z' 'A'-'Z']+
let string = '"' [^ '"']* '"'

rule token = parse
| whitespace {token lexbuf}
| "/*" { comment lexbuf}
| "(" { LEFT_PAREN }
| ")" { RIGHT_PAREN }
| "{" {LEFT_BRACE}
| "}" {RIGHT_BRACE}
| "," {COMMA}
| "." {DOT}
| "-" {MINUS}
| "+" {PLUS}
| ";" {SEMICOLON}
| "/" {SLASH}
| "*" {STAR}
| "!" {BANG}
| "!=" {BANG_EQUAL}
| "="{EQUAL}
| "==" {EQUAL_EQUAL}
| ">" {GREATER}
| ">=" {GREATER_EQUAL}
| "<" {LESS}
| "<=" {LESS_EQUAL}
| "&&" {AND}
| "||" {OR}
| "true" {TRUE}
| "false" {FALSE}
| "if" {IF}
| "nil" {NIL}
| "var" {VAR}
| digit as lxm { NUMBER(float_of_string lxm) }
| identifier as lxm { IDENTIFIER(lxm) }
| string as lxm { STRING(Scanf.unescaped(String.sub lxm 1 ((String.length lxm) - 2)))  }
| eof { EOF }
| _ as lxm { raise @@ SyntaxError ("Illegal character " ^ (String.make 1 lxm)) }

and comment = parse
| "*/" {token lexbuf}
| newline { next_line lexbuf; comment lexbuf }
| eof { raise @@ SyntaxError "Unexpected EOF during comment "}
| _ { comment lexbuf }