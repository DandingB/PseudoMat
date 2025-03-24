open Lexing
open Parser
open Lexer
open Ast 
open Interp

let rec printTree expr =
  match expr with
  | Ecst (Cint x) -> Printf.printf "Ecst %d\n" x
  | Ebinop (op, e1, e2) ->
      printTree e1;
      (match op with
      | Badd -> Printf.printf " + "
      | Bsub -> Printf.printf " - "
      | Bmul -> Printf.printf " * "
      | Bdiv -> Printf.printf " / "
      | _ -> Printf.printf " ?");
      printTree e2;
      Printf.printf "\n"
  | _ -> Printf.printf "Unknown expression\n"

let () =
  let c = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel c in
  let result = Parser.main Lexer.token lexbuf in
  Interp.stmt result