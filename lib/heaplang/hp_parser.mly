%{
open Hp_ast

let gen_rec fname ids tys expr =
if List.length ids <> List.length tys - 1 then
  failwith "gen_rec: number of identifiers and types do not match"
else
  let rec aux ids typs =
      match ids with
      | [] -> Rec (BAnon, BAnon, expr, TFun typs)
      | [ id ] -> Rec (BAnon, BNamed id, expr, TFun typs)
      | id :: rest -> Rec (BAnon, BNamed id, aux rest (List.tl tys), TFun typs)
    in
      match ids with
      | [] ->  Rec (fname, BAnon , expr, TFun tys)
      | [ id ] -> Rec (fname, BNamed id, expr, TFun tys)
      | id :: ids -> Rec (fname, BNamed id, aux ids (List.tl tys), TFun tys)

%}

%token <int> INT
%token <string> IDENT
%token TRUE FALSE UNIT
%token IF THEN ELSE
%token LET IN REC VAL DEFINITION COLON INTTYPE VOID PAIR FUN
%token LAMBDA REF
%token PLUS MINUS TIMES DIV
%token EQ LT AND OR
%token LPAREN RPAREN DOT COMMA FIRST SECOND
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
  | DEFINITION IDENT COLON VAL LPAREN TIMES types TIMES RPAREN COLON
    EQ REC idents COLON EQ expr %prec LOW_PRECEDENCE { gen_rec (BNamed $2) (List.tl $13) $7 $16 }

types:
  | types COMMA typ          { $1 @ [ $3 ] }
  | typ                        { [ $1 ] }

typ:
  | INTTYPE                     { TInt }
  | VOID                        { TUnit }
  | PAIR LPAREN typ COMMA typ RPAREN  { TPair ($3, $5) }
  | FUN LPAREN types RPAREN  { TFun $3 }

idents:
  | IDENT                          { [ $1 ] }
  | idents IDENT             { $1 @ [$2] }

atom:
  | IDENT                          { Var($1) }
  | INT                            { Val (LitV (LitInt $1)) }
  | TRUE                           { Val (LitV (LitBool true)) }
  | FALSE                          { Val (LitV (LitBool false)) }
  | UNIT                           { Val (LitV LitUnit) }
  | LPAREN expr COMMA expr RPAREN  { Pair($2, $4) }
  | LPAREN expr RPAREN             { $2 }
  | FIRST expr  %prec LOW_PRECEDENCE                   { Fst $2 }
  | SECOND expr    %prec LOW_PRECEDENCE                { Snd $2 }