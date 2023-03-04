%{ 
    open Ast
%}

%token <float> NUMBER 
%token <string> IDENTIFIER STRING 
%token TRUE FALSE
%token STAR SLASH
%token PLUS MINUS

%token RIGHT_PAREN LEFT_BRACE RIGHT_BRACE COMMA SEMICOLON LEFT_PAREN
%token BANG BANG_EQUAL EQUAL EQUAL_EQUAL GREATER GREATER_EQUAL 
%token LESS LESS_EQUAL AND FUN 
%token IF NIL OR PRINT VAR WHILE EOF
%start program

%type <Ast.value> value
%type <Ast.stmt list> program

%%

value:
| NIL { Nil }
| n = NUMBER { Number n }
| TRUE { Boolean true }
| FALSE { Boolean false }
| s = STRING { String s }
| i = IDENTIFIER { Variable i }

(* TODO: This is the case for most of my grammar rules.
  How do i translate the following:
  unary          → ( "!" | "-" ) unary | call ;
  call           → primary ( "(" arguments? ")" )* ;
  Specifically, zero or many parens containing arguments. I can easily do 1 list of arguments,
  but I want to support something like getfunction()() where the second parans invokes the 
  function the first pair of parens returns *)
invocation:
| v = value { Value($startpos, v) }
| i = IDENTIFIER LEFT_PAREN args = separated_list(COMMA, expression) RIGHT_PAREN
  { Invocation($startpos, i, args) }

unary:
| i = invocation { i }
| BANG u = unary { Not($startpos, u) }
| MINUS u = unary { Negate($startpos, u) }

factor:
| u = unary { u }
| u1 = unary SLASH u2 = unary { Divide($startpos, u1, u2) }
| u1 = unary STAR u2 = unary { Multiply($startpos, u1, u2) }

term:
| f = factor { f }
| f1 = factor MINUS f2 = factor { Subtract($startpos, f1, f2) }
| f1 = factor PLUS f2 = factor { Plus($startpos, f1, f2) }

comparison:
| t = term { t }
| t1 = term LESS t2 = term { Less($startpos, t1, t2) }
| t1 = term GREATER t2 = term { Greater($startpos, t1, t2) }
| t1 = term LESS_EQUAL t2 = term { LessEqual($startpos, t1, t2) }
| t1 = term GREATER_EQUAL t2 = term { GreaterEqual($startpos, t1, t2) }

equality:
| c = comparison { c }
| c1 = comparison BANG_EQUAL c2 = comparison { NotEquals($startpos, c1, c2) }
| c1 = comparison EQUAL_EQUAL c2 = comparison { Equals($startpos, c1, c2) }

logical_and:
| e1 = equality AND e2 = equality { And($startpos, e1, e2) }


logical_or:
| b1 = logical_and OR b2 = logical_and { Or($startpos, b1, b2) }

assignment:
| ident = IDENTIFIER EQUAL e = expression { Assignment($startpos, ident, e) }
| e = equality { e }
| l = logical_or { l }

expression:
| a = assignment { a }
| LEFT_PAREN e = expression RIGHT_PAREN { e }

block:
| LEFT_BRACE decls = list(declaration) RIGHT_BRACE {  Block($startpos, decls) }

statement:
| e = expression SEMICOLON { Expression($startpos, e) }
| b = block { b }
| PRINT e = expression SEMICOLON { Print($startpos, e) }
| IF LEFT_PAREN cond = expression RIGHT_PAREN body = statement { If($startpos, cond, body, None) }
| IF LEFT_PAREN cond = expression RIGHT_PAREN iftrue = statement iffalse = statement
  { If($startpos, cond, iftrue, Some iffalse) }
| WHILE LEFT_PAREN cond = expression RIGHT_PAREN stmt = statement { While($startpos, cond, stmt) }
 
declaration:
| stmt = statement { stmt }
(* Unlike jlox in the book we're going to mandate that all vars have a value on intialization *)
| VAR ident = IDENTIFIER EQUAL e = expression SEMICOLON { Declaration($startpos, ident, Some e) }
| VAR ident = IDENTIFIER SEMICOLON { Declaration($startpos, ident, None) }
| FUN funname = IDENTIFIER LEFT_PAREN params = separated_list(COMMA, IDENTIFIER) RIGHT_PAREN b = block
  { Function($startpos, funname, params, b) }

program:
| decls = separated_list(SEMICOLON, declaration) EOF { decls }
