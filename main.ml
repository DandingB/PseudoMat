open Lexing
open Parser
open Lexer

let () = 
    let c = open_in Sys.argv.(1) in 
    let lexbuf = Lexing.from_channel c in
    let result = Parser.main Lexer.token lexbuf in
    Printf.printf "%d" result
