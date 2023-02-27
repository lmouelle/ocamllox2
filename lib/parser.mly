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
%token LESS LESS_EQUAL AND CLASS FUN FOR 
%token IF NIL OR PRINT RETURN SUPER THIS VAR WHILE EOF
%start program

%type <Ast.value> value
%type <Ast.expr> expr
%type <Ast.expr list> program

%left PLUS MINUS 
%left STAR SLASH
%left AND OR
%left EQUAL EQUAL_EQUAL BANG_EQUAL GREATER GREATER_EQUAL LESS LESS_EQUAL PRINT
%right BANG
%%

value:
| NIL { Nil }
| n = NUMBER { Number n }
| TRUE { Boolean true }
| FALSE { Boolean false }
| s = STRING { String s }
| i = IDENTIFIER { Variable i }

expr:
| v = value { Value ($startpos, v) }
| IF LEFT_PAREN expr RIGHT_PAREN LEFT_BRACE expr RIGHT_BRACE LEFT_BRACE expr RIGHT_BRACE
  { If ($startpos,$3,$6,$9) }
| expr PLUS expr { Plus ($startpos,$1,$3) }
| expr MINUS expr { Subtract ($startpos,$1,$3) }
| expr SLASH expr { Divide ($startpos,$1,$3) }
| expr STAR expr { Multiply ($startpos,$1,$3) }
| expr OR expr { Or ($startpos,$1,$3) }
| expr AND expr { And ($startpos,$1,$3) }
| VAR ident = IDENTIFIER EQUAL e = expr { Assignment($startpos, ident, e) }
| ident = IDENTIFIER EQUAL e = expr { Mutation($startpos, ident, e) }
| expr EQUAL_EQUAL expr { Equals($startpos,$1, $3) }
| expr BANG_EQUAL expr { NotEquals($startpos,$1, $3) }
| expr GREATER expr { Greater($startpos,$1, $3) }
| expr GREATER_EQUAL expr { GreaterEqual($startpos,$1, $3) }
| expr LESS expr { Less($startpos,$1, $3) }
| expr LESS_EQUAL expr { LessEqual($startpos,$1, $3) }
| LEFT_PAREN expr RIGHT_PAREN { Grouping($startpos, $2) }
| BANG expr { Not($startpos, $2) }
| PRINT expr { Print($startpos, $2) }

exprs:
| (* empty *) { [] }
| expr { [$1] }
| expr SEMICOLON exprs { $1 :: $3 }

program:
| exprs EOF { $1 }