open Ast
open Format

type value =
  | Vnone
  | Vbool of bool
  | Vnum of float
  | Vstring of string
  | Vlist of value array
  
let rec expr = function 
  | Ecst (Cnum n) -> Vnum n
  | Ecst (Cbool n) -> Vbool n
  | Ecst (Cstring n) -> Vstring n
  | Ebinop (Badd | Bsub | Bmul | Bdiv | Blt | Beq | Bgt | Bge | Ble | Bneq | Band | Bor  as op, e1, e2) ->
      let v1 = expr e1 in
      let v2 = expr e2 in
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
  | Eunop (Uneg as op, e) ->
    let v1 = expr e in
    begin match v1 with
    | Vnum n2 -> Vnum( -.n2 )
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

  (* let is_false = function
  | Vnone
  | Vbool false
  | Vstring "" 
  | Vlist [||] -> true
  | Vnum n -> n = 0
  | _ -> false 

let is_true v = not (is_false v)  *)


let rec stmt = function
 | Sprint e -> print_value (expr e)
 | Sblock stmts -> List.iter stmt stmts
 | Sif (e, stmts) -> 
    begin match (expr e) with
      | Vbool e -> if e then stmt stmts 
      | _ -> failwith "Not boolean"
    end


let file (dl) =
  stmt dl


(*  | Sif (e, s1, s2) ->
    if is_true (expr ctx e) then stmt ctx s1 else stmt ctx s2 (* DONE (question 2) *)
  | Sassign ({id}, e1) -> *)

