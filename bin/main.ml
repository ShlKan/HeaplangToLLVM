open Heaplang
open Llvmir
open Translation

let () =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s (--ast|--llvm) <filename>\n" Sys.argv.(0)
  else
    let mode = Sys.argv.(1) in
    let filename = Sys.argv.(2) in
    try
      match mode with
      | "--ast" ->
          let stmts = Hp_read.hp_read filename in
          Format.printf "AST:\n%s\n"
            (String.concat "\n" (List.map Hp_ast.ast_to_string stmts))
      | "--llvm" ->
          let expr = Hp_read.hp_read filename in
          let llvm_ast = Translate.translateHeaplang expr in
          Format.printf
            "declare i8* @malloc(i64)\n\
             declare i32 @printf(i8*, ...)\n\
             %s.str = private constant [4 x i8] %s\n\
             %s"
            "@" "c\"%d\\0A\\00\""
            (String.concat "\n" (List.map Llvm_ast.func_to_string llvm_ast))
      | _ -> Printf.eprintf "Unknown mode: %s. Use --ast or --llvm.\n" mode
    with
    | Failure msg -> prerr_endline ("Error: " ^ msg)
    | Sys_error msg -> prerr_endline ("System error: " ^ msg)
