
let hp_read filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  try
    let result = Hp_parser.main Hp_lexer.read lexbuf in
    close_in channel;
    Format.printf "AST:\n %s" (Hp_ast.ast_to_string result)
  with
  | Hp_parser.Error ->
      let pos = lexbuf.lex_curr_p in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol + 1 in
      close_in channel;
      failwith (Printf.sprintf "Syntax error at line %d, column %d" line col)
  | exn ->
      close_in channel;
      raise exn