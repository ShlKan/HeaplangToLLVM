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
  | "()"                  { UNIT }
  | "if:"                  { IF }
  | "then"                { THEN }
  | "else"                { ELSE }
  | "let:"                { LET }
  | "ref"                 { REF }
  | "Definition"          { DEFINITION }
  | "val"                 { VAL }
  | "rec:"                 { REC }
  | "in"                  { IN }
  | "λ" | "fun"           { LAMBDA }
  | "="                   { EQ }
  | "<"                   { LT }
  | "&&"                  { AND }
  | "||"                  { OR }
  | "+"                   { PLUS }
  | "-"                   { MINUS }
  | "*"                   { TIMES }
  | "/"                   { DIV }
  | "."                   { DOT }
  | "("                   { LPAREN }
  | ")"                   { RPAREN }
  | ":"                   { COLON }
  | "#" (digit as i)        { INT (int_of_string i) }
  | ident as id           { IDENT id }
  | eof                   { EOF }
  | _                     { failwith "Unexpected character" }
