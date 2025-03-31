open Ast
open Format



type value =
  | Vnone
  | Vbool of bool
  | Vnum of float
  | Vstring of string
  | Vlist of value array

(* Local variables (function parameters and local variables introduced
  by assignments) are stored in a hash table that is passed to the
  following OCaml functions as parameter `ctx`. *)

type ctx = (string, value) Hashtbl.t
  
let rec expr ctx = function 
  | Ecst (Cnum n) -> Vnum n
  | Ecst (Cbool n) -> Vbool n
  | Ecst (Cstring n) -> Vstring n
  | Ebinop (Badd | Bsub | Bmul | Bdiv | Blt | Beq | Bgt | Bge | Ble | Bneq | Band | Bor  as op, e1, e2) ->
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      begin match op, v1, v2 with
        | Badd, Vnum n1, Vnum n2 -> Vnum (n1 +. n2)
        | Bsub, Vnum n1, Vnum n2 -> Vnum (n1 -. n2)
        | Bmul, Vnum n1, Vnum n2 -> Vnum (n1 *. n2)
        | Bdiv, Vnum n1, Vnum n2 -> Vnum (n1 /. n2)
        | Blt, Vnum n1, Vnum n2 -> Vbool (n1 < n2)
        | Beq, Vnum n1, Vnum n2 -> Vbool (n1 == n2)
        | Bgt, Vnum n1, Vnum n2 -> Vbool (n1 > n2)
        | Bge, Vnum n1, Vnum n2 -> Vbool (n1 >= n2)
        | Ble, Vnum n1, Vnum n2 -> Vbool (n1 <= n2)
        | Bneq, Vnum n1, Vnum n2 -> Vbool (n1 != n2)
        | Band, Vbool n1, Vbool n2 -> Vbool (n1 && n2)
        | Bor, Vbool n1, Vbool n2 -> Vbool (n1 || n2)
        | _ -> failwith "Invalid binary operation"
      end
  | Eunop (Uneg | Unot as op, e) ->
    let v1 = expr ctx e in
    begin match op, v1 with
    | Uneg, Vnum n2 -> Vnum( -.n2 )
    | _ -> failwith "Unsupported statement"
    end
  | _ -> failwith "Unsupported expression"


let has_decimal_part x =
  x <> floor x  

let print_value e = 
  match e with
  | Vnum n ->
    if has_decimal_part n 
    then Printf.printf "%f\n" n
    else Printf.printf "%d\n" (int_of_float n)
  | Vbool n -> Printf.printf "%B\n" n
  | Vstring n -> Printf.printf "%s\n" n
  | _ -> failwith "Unsupported statement"

  (* let is_false = function
  | Vnone
  | Vbool false
  | Vstring "" 
  | Vlist [||] -> true
  | Vnum n -> n = 0
  | _ -> false 

let is_true v = not (is_false v)  *)


(* stmts is all the statements in the block. *)
let rec stmt ctx = function
 | Sprint e -> print_value (expr ctx e)
 | Sblock stmts -> block ctx stmts
 | Sif (e, bl1, bl2) -> 
    let e1 = expr ctx e in
    begin match e1 with
      | Vbool e1 -> if e1 then stmt ctx bl1 else stmt ctx bl2 
      | _ -> failwith "Not boolean"
    end
  | _ -> failwith "Unsupported statement"
and block ctx = function
    | [] -> ()
    | s :: sl -> stmt ctx s; block ctx sl


let file (dl) =
  stmt (Hashtbl.create 17) dl


(*  | Sif (e, s1, s2) ->
    if is_true (expr ctx e) then stmt ctx s1 else stmt ctx s2 (* DONE (question 2) *)
  | Sassign ({id}, e1) -> *)

