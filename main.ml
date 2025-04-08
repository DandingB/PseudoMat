open Lexing
open Parser
open Lexer
open Ast 
open Interp

let () =
  let c = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel c in
  let result = Parser.main Lexer.next_token lexbuf in
  Interp.file result

