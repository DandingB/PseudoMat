open Lexing
open Parser
open Lexer
open Ast 
open Interp

(* let () =
  let c = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel c in
  let result = Parser.main Lexer.next_token lexbuf in

  Interp.file result *)

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
    let pos = Lexing.lexeme_start_p lexbuf in
    Printf.eprintf "Lexing error at line %d, column %d: %s\n"
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
      msg
(* If parser throws error then display line and column of the error *)
| Parser.Error ->
    let pos = Lexing.lexeme_start_p lexbuf in
    Printf.eprintf "Syntax error at line %d, column %d\n"
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
(* If intepreter throws error then display the error message *)
| Failure msg ->
    Printf.eprintf "Error: %s\n" msg
