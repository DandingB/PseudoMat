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
    Printf.eprintf "Syntax error\n"
(* If intepreter throws error then display the error message *)
| Failure msg ->
    Printf.eprintf "Error: %s\n" msg
