open Lexing
open Parser
open Lexer
open Ast 
open Interp

let rec printExpr = function
| Ecst (Cint n) -> "Cint " ^ string_of_int n
| Ebinop (op, e1, e2) ->
    let e1 = printExpr e1 in
    let e2 = printExpr e2 in
    "Ebinop {" ^ e1 ^ "} {" ^ e2 ^ "}"


let () =
  let c = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel c in
  let result = Parser.main Lexer.next_token lexbuf in
  Interp.file result