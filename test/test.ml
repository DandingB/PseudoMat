(* Ignore specific warnings *)
[@@@ocaml.warning "-33"]

open Lib
open Lexing
open Parser
open Lexer
open Ast 
open Interp

(* let test msg op x y = Printf.printf "%s: %d\n\n" msg (op x y)
 *)
let get_token_list lexbuf = 
  let rec work acc =
    match Lexer.next_token lexbuf with
    | EOF -> acc
    | t -> work (t::acc)
  in List.rev (work[])

let () =
  let lexbuf = Lexing.from_string "Let h be \"hej\" as string" in
  let token_list = get_token_list lexbuf in
  Printf.printf "%d\n" (List.length token_list)