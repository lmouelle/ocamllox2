%{ 
    open Ast
%}

%token <float> NUMBER 
%token <string> IDENTIFIER STRING 
%token TRUE FALSE
%token RIGHT_PAREN LEFT_BRACE RIGHT_BRACE COMMA DOT MINUS PLUS SEMICOLON LEFT_PAREN
%token SLASH STAR BANG BANG_EQUAL EQUAL EQUAL_EQUAL GREATER GREATER_EQUAL 
%token LESS LESS_EQUAL AND CLASS ELSE BOOL FUN FOR 
%token IF NIL OR PRINT RETURN SUPER THIS VAR WHILE EOF
%token COMMENT_START COMMENT_END
%start value

%type <Ast.untyped_value> value

%%

value:
| NIL { Nil }
| n = NUMBER { Number n }
| TRUE { Boolean true }
| FALSE { Boolean false }
| s = STRING { String s }
| i = IDENTIFIER { Variable i }

