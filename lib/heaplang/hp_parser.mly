%{
open Hp_ast
%}

%token <int> INT
%token <string> IDENT
%token TRUE FALSE UNIT
%token IF THEN ELSE
%token LET IN
%token LAMBDA REF
%token PLUS MINUS TIMES DIV
%token EQ LT AND OR
%token LPAREN RPAREN DOT
%token EOF

%left OR
%left AND
%left EQ LT
%left PLUS MINUS
%left TIMES DIV
%nonassoc LOW_PRECEDENCE

%start <expr> main
%type <expr> expr

%%

main:
  | expr EOF { $1 }

expr:
  | stmt_expr                  { $1 }
  | bin_expr                   { $1 }
  | unary_expr                 { $1 }
  | atom                       { $1 }

bin_expr:
  | expr OR expr                   { BinOp(OrOp, $1, $3) }
  | expr AND expr                  { BinOp(AndOp, $1, $3) }
  | expr EQ expr                   { BinOp(EqOp, $1, $3) }
  | expr LT expr                   { BinOp(LtOp, $1, $3) }
  | expr PLUS expr                 { BinOp(PlusOp, $1, $3) }
  | expr MINUS expr                { BinOp(MinusOp, $1, $3) }
  | expr TIMES expr                { BinOp(MultOp, $1, $3) }
  | expr DIV expr                  { BinOp(QuotOp, $1, $3) }

unary_expr:
  | REF expr %prec LOW_PRECEDENCE                     { AllocN(Val (LitV (LitInt 1)), $2) }

stmt_expr:
  | LET IDENT EQ expr IN expr %prec LOW_PRECEDENCE      { Let(Var $2, $4, $6) }
  | IF expr THEN expr ELSE expr %prec LOW_PRECEDENCE    { If($2, $4, $6) }
  | LAMBDA IDENT DOT expr  %prec LOW_PRECEDENCE        { Rec(BAnon, BNamed $2, $4) }

atom:
  | IDENT                          { Var($1) }
  | INT                            { Val (LitV (LitInt $1)) }
  | TRUE                           { Val (LitV (LitBool true)) }
  | FALSE                          { Val (LitV (LitBool false)) }
  | UNIT                           { Val (LitV LitUnit) }
  | LPAREN expr RPAREN             { $2 }