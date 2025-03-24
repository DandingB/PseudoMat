open Lexing
open Parser
open Lexer

let () = 
    let c = open_in Sys.argv.(1) in 
    let lexbuf = Lexing.from_channel c in
    let result = Parser.main Lexer.token lexbuf in

let rec printTree = function 
  | Ecst x -> Printf.printf "Ecst %d" x
  | Ebinop (op, e1, e2) ->
      Printf.printf "Binop {";
      printTree e1;
      Printf.printf " %s " op;
      printTree e2;
      Printf.printf "}"