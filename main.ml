open Lexing
open Parser
open Lexer

let () = 
    let C = open_in sys.argv.(1) in 
    let lexbuf = Lexing.from_channel C in
    let result = Parser.main Lexer.token lexbuf in
    Printf.printf "%d", result
