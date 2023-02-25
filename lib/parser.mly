%{ 
    open Ast
%}

%token <float> NUMBER 
%token <string> IDENTIFIER STRING 
%token TRUE FALSE
%token STAR SLASH
%token PLUS MINUS

%token RIGHT_PAREN LEFT_BRACE RIGHT_BRACE COMMA DOT SEMICOLON LEFT_PAREN
%token BANG BANG_EQUAL EQUAL EQUAL_EQUAL GREATER GREATER_EQUAL 
%token LESS LESS_EQUAL AND CLASS ELSE FUN FOR 
%token IF NIL OR PRINT RETURN SUPER THIS VAR WHILE EOF
%token COMMENT_START COMMENT_END
%start program

%type <Ast.value> value
%type <Ast.expr> expr
%type <Ast.expr list> program

%left PLUS MINUS 
%left STAR SLASH
%left AND OR
%left EQUAL EQUAL_EQUAL BANG_EQUAL GREATER GREATER_EQUAL LESS LESS_EQUAL

%%

value:
| NIL { Nil }
| n = NUMBER { Number n }
| TRUE { Boolean true }
| FALSE { Boolean false }
| s = STRING { String s }
| i = IDENTIFIER { Variable i }

expr:
| v = value { Value v }
| IF LEFT_PAREN expr RIGHT_PAREN LEFT_BRACE expr RIGHT_BRACE LEFT_BRACE expr RIGHT_BRACE
  { If ($3,$6,$9) }
| expr PLUS expr { Plus ($1,$3) }
| expr MINUS expr { Subtract ($1,$3) }
| expr SLASH expr { Divide ($1,$3) }
| expr STAR expr { Multiply ($1,$3) }
| expr OR expr { Or ($1,$3) }
| expr AND expr { And ($1,$3) }
| VAR ident = IDENTIFIER EQUAL e = expr { Assignment(ident, e) }
| expr EQUAL_EQUAL expr { Equals($1, $3) }
| expr BANG_EQUAL expr { NotEquals($1, $3) }
| expr GREATER expr { Greater($1, $3) }
| expr GREATER_EQUAL expr { GreaterEqual($1, $3) }
| expr LESS expr { Less($1, $3) }
| expr LESS_EQUAL expr { LessEqual($1, $3) }

exprs:
| (* empty *) { [] }
| expr { [$1] }
| expr SEMICOLON exprs { $1 :: $3 }

program:
| exprs EOF { $1 }