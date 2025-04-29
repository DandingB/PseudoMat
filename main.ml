open Lib
open Lexing
open Parser
open Lexer
open Ast 
open Interp

let () =
(* Read characters from file *)
let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
try
  (* Run all the different tokens through the parser *)
  let ast = Parser.main Lexer.next_token lexbuf in
  (* Run the file function from the interp.ml file on the parsed output. *)
  Interp.file ast
with
(* If lexer throughts error, then display line and column and character *)
| Lexer.Lexing_error msg ->
    Printf.eprintf "Lexing error: '%s'\n"
      msg
(* If parser throws error then display line and column of the error *)
| Parser.Error ->
    Printf.eprintf "Syntax error at line %d\n" lexbuf.lex_curr_p.pos_lnum
(* If intepreter throws error then display the error message *)
| Failure msg ->
    Printf.eprintf "Runtime error: %s\n" msg
