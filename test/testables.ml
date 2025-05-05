(* This module defines the tokens and AST as testable data types & how to print them. *)

[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-9"]

open Lib
open Parser
open Ast

let pp_token fmt p = 
  match p with
  | WHILE -> Format.fprintf fmt "WHILE"
  | TRUE -> Format.fprintf fmt "TRUE"
  | TO -> Format.fprintf fmt "TO"
  | SUB -> Format.fprintf fmt "SUB"
  | STRING s -> Format.fprintf fmt "STRING(\"%s\")" s
  | SEMICOLON -> Format.fprintf fmt "SEMICOLON"
  | RSQ -> Format.fprintf fmt "RSQ"
  | RP -> Format.fprintf fmt "RP"
  | RETURN -> Format.fprintf fmt "RETURN"
  | RC -> Format.fprintf fmt "RC"
  | PRINT -> Format.fprintf fmt "PRINT"
  | POW -> Format.fprintf fmt "POW"
  | OR -> Format.fprintf fmt "OR"
  | NUMBER f -> Format.fprintf fmt "NUMBER(%f)" f
  | NOTEQUALS -> Format.fprintf fmt "NOTEQUALS"
  | NOT -> Format.fprintf fmt "NOT"
  | MUL -> Format.fprintf fmt "MUL"
  | MOD -> Format.fprintf fmt "MOD"
  | LSQ -> Format.fprintf fmt "LSQ"
  | LP -> Format.fprintf fmt "LP"
  | LET -> Format.fprintf fmt "LET"
  | LESSEQUALS -> Format.fprintf fmt "LESSEQUALS"
  | LESS -> Format.fprintf fmt "LESS"
  | LENGTH -> Format.fprintf fmt "LENGTH"
  | LC -> Format.fprintf fmt "LC"
  | IF -> Format.fprintf fmt "IF"
  | ID s -> Format.fprintf fmt "ID('%s')" s
  | GREATEREQUALS -> Format.fprintf fmt "GREATEREQUALS"
  | GREATER -> Format.fprintf fmt "GREATER"
  | FUNCTION -> Format.fprintf fmt "FUNCTION"
  | FOR -> Format.fprintf fmt "FOR"
  | FALSE -> Format.fprintf fmt "FALSE"
  | EQUALS -> Format.fprintf fmt "EQUALS"
  | EOF -> Format.fprintf fmt "EOF"
  | ELSEIF -> Format.fprintf fmt "ELSEIF"
  | ELSE -> Format.fprintf fmt "ELSE"
  | DOT -> Format.fprintf fmt "DOT"
  | DIV -> Format.fprintf fmt "DIV"
  | DIMENSION (f1, f2) -> Format.fprintf fmt "DIMENSION(%f,%f)" f1 f2
  | DATATYPE s -> Format.fprintf fmt "DATATYPE('%s')" s
  | COMMA -> Format.fprintf fmt "COMMA"
  | BE -> Format.fprintf fmt "BE"
  | ASSIGN -> Format.fprintf fmt "ASSIGN"
  | AS -> Format.fprintf fmt "AS"
  | AND -> Format.fprintf fmt "AND"
  | ADD -> Format.fprintf fmt "ADD"

let equal_token p1 p2 = 
  if p1 = p2 then true else false

let testable_token = Alcotest.testable pp_token equal_token

let pp_unop op =
  match op with
  | Uneg -> "Uneg"
  | Unot -> "Unot"

let pp_binop op =
  match op with
  | Badd -> "Badd" | Bsub -> "Bsub" | Bmul -> "Bmul" | Bdiv -> "Bdiv"
  | Bmod -> "Bmod" | Bpow -> "Bpow" | Beq -> "Beq" | Bneq -> "Bneq"
  | Blt -> "Blt" | Ble -> "Ble" | Bgt -> "Bgt" | Bge -> "Bge"
  | Band -> "Band" | Bor -> "Bor"

let pp_constant const =
  match const with
  | Cnone -> Printf.sprintf "Cnone"
  | Cbool b -> Printf.sprintf "Cbool (%b)" b
  | Cstring str ->  Printf.sprintf "Cstring (\"%s\")" str
  | Cnum fl -> Printf.sprintf "Cnum (%f)" fl

let rec pp_expr expr =
  match expr with
  | Ecst const                    -> Printf.sprintf "Ecst (%s)" (pp_constant const)
  | Eident {id}                   -> Printf.sprintf "Eident (%s)" id
  | Ebinop (binop, expr1, expr2)  -> Printf.sprintf "Ebinop (%s, %s, %s)" (pp_binop binop) (pp_expr expr1) (pp_expr expr2)
  | Eunop (unop, expr)            -> Printf.sprintf "Eunop (%s, %s)" (pp_unop unop) (pp_expr expr)
  | Ecall ({id=func_id}, el) ->
    let args = String.concat ", " (List.map pp_expr el) in
    Printf.sprintf "Ecall (%s, [%s])" func_id args
  | Earray el ->
    let elems = String.concat ", " (List.map pp_expr el) in
    Printf.sprintf "Earray ([%s])" elems
  | Eget (e1, e2)                 -> Printf.sprintf "Eget (%s, %s)" (pp_expr e1) (pp_expr e2)
  | Egetmatrix (e1, e2, e3)       -> Printf.sprintf "Egetmatrix (%s, %s, %s)" (pp_expr e1) (pp_expr e2) (pp_expr e3)
  | Ematrix ell ->
      let rows = List.map (fun row ->
        Printf.sprintf "[%s]" (String.concat ", " (List.map pp_expr row))
      ) ell in
      Printf.sprintf "Ematrix ([%s])" (String.concat ", " rows)
  | Elength e                      -> Printf.sprintf "Elength (%s)" (pp_expr e)

let rec pp_stmt p = 
  match p with
  | Sif (expr, stmt1, stmt2)          -> Printf.sprintf "Sif (%s, %s)" (pp_expr expr) (pp_stmt stmt1)
  | Selseif (expr, stmt1, stmt2)      -> Printf.sprintf "Selseif (%s, %s)" (pp_expr expr) (pp_stmt stmt1)
  | Sreturn expr                      -> Printf.sprintf "Sreturn (%s)" (pp_expr expr)
  | Sassign ({id}, expr, str)         -> Printf.sprintf "Sassign (%s, %s, %s)" id (pp_expr expr) str
  | Sprint expr                       -> Printf.sprintf "Sprint (%s)" (pp_expr expr)
  | Sblock stmts                      -> Printf.sprintf "Sblock ([%s])" (block stmts)
  | Sfor ({id}, expr1, expr2, stmt1, stmt2) -> Printf.sprintf "Sfor (%s, %s, %s, %s, %s)" id (pp_expr expr1) (pp_expr expr2) (pp_stmt stmt1) (pp_stmt stmt2)
  | Swhile (expr, stmt)               -> Printf.sprintf "Swhile (%s, %s)" (pp_expr expr) (pp_stmt stmt) 
  | Srange (expr1, expr2, stmt)       -> Printf.sprintf "Srange (%s, %s, %s)" (pp_expr expr1) (pp_expr expr2) (pp_stmt stmt) 
  | Seval expr                        -> Printf.sprintf "Seval (%s)" (pp_expr expr)
  | Sset (expr1, expr2, expr3)        -> Printf.sprintf "Sset (%s, %s, %s)" (pp_expr expr1) (pp_expr expr2) (pp_expr expr3) 
  | Ssetmatrix (expr1, expr2, expr3, expr4) -> Printf.sprintf "Ssetmatrix (%s, %s, %s, %s)" (pp_expr expr1) (pp_expr expr2) (pp_expr expr3) (pp_expr expr4)
  | Sfunc ({id}, idList, stmt) ->
    let args = String.concat ", " (List.map (fun {id} -> id) idList) in
    Printf.sprintf "Sfunc (%s, [%s], %s)" id args (pp_stmt stmt)

and block = function
  | [] -> ""
  | s :: sl -> pp_stmt s ^ "; " ^ block sl


let pp_ast fmt p = 
  Format.fprintf fmt "%s" (pp_stmt p)

let equal_ast p1 p2 = 
  if p1 = p2 then true else false

let testable_ast = Alcotest.testable pp_ast equal_ast


