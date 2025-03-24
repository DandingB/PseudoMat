open Ast
open Format

type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

let rec expr = function 
  | Ecst (Cint n) -> Vint n
  | Ebinop (Badd | Bsub | Bmul | Bdiv as op, e1, e2) ->
      let v1 = expr e1 in
      let v2 = expr e2 in
      begin match op, v1, v2 with
        | Badd, Vint n1, Vint n2 -> Vint (n1 + n2)
        | Bsub, Vint n1, Vint n2 -> Vint (n1 - n2)
        | Bmul, Vint n1, Vint n2 -> Vint (n1 * n2)
        | _ -> failwith "Invalid binary operation"
      end
  | _ -> failwith "Unsupported expression"
    


