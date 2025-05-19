{
open Hp_parser
}

let digit = ['0'-'9']+
let ident = ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]*

rule read = parse
  | [' ' '\t' '\n']       { read lexbuf }
  | "\"" ([^ '"']*) "\"" as s {
      let len = String.length s in
      IDENT (String.sub s 1 (len - 2))  (* strip quotes *)
    }
  | "true"                { TRUE }
  | "false"               { FALSE }
  | "#()"                  { UNIT }
  | "if:"                  { IF }
  | "then"                { THEN }
  | "else"                { ELSE }
  | "Fst"               { FIRST }
  | "Snd"               { SECOND }
  | "let:"                { LET }
  | "ref"                 { REF }
  | "print"              { PRINT }
  | "Definition"          { DEFINITION }
  | "val"                 { VAL }
  | "λ:"                  { LAMBDA }
  | "rec:"                 { REC }
  | "in"                  { IN }
  | "int"                { INTTYPE }
  | "void"                { VOID }
  | "pair"                { PAIR }
  | "loc"                { LOC }
  | "fun"                { FUN }
  | "λ" | "fun"           { LAMBDA }
  | "="                   { EQ }
  | "<-"                   { ASSIGN }
  | "<"                   { LT }
  | "&&"                  { AND }
  | "||"                  { OR }
  | "+"                   { PLUS }
  | "-"                   { MINUS }
  | "*"                   { TIMES }
  | "/"                   { DIV }
  | "."                   { DOT }
  | ","                   { COMMA }
  | "("                   { LPAREN }
  | ")"                   { RPAREN }
  | ":"                   { COLON }
  | "!"                   { BANG }
  | ";;"                  { SEMICOLON }
  | "#" (digit as i)        { INT (int_of_string i) }
  | ident as id           { IDENT id }
  | eof                   { EOF }
  | _                     { failwith "Unexpected character" }
