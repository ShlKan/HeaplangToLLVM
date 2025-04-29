open Heaplang

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    try
      Hp_read.hp_read filename
    with
    | Failure msg -> prerr_endline ("Error: " ^ msg)
    | Sys_error msg -> prerr_endline ("System error: " ^ msg)