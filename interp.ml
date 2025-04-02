open Ast
open Format

type value =
  | Vnone
  | Vbool of bool
  | Vnum of float
  | Vstring of string
  | Varray of value array

(* Local variables (function parameters and local variables introduced
  by assignments) are stored in a hash table that is passed to the
  following OCaml functions as parameter `ctx`. *)

type ctx = (string, value) Hashtbl.t

let has_decimal_part x =
  x <> floor x  

let rec to_string e =
  match e with
  | Vnone -> Vstring "None"
  | Vnum n ->
    if has_decimal_part n 
    then Vstring(Printf.sprintf "%f" n)
    else Vstring(Printf.sprintf "%d" (int_of_float n))
  | Vbool n -> Vstring(Printf.sprintf "%B" n)
  | Vstring n -> Vstring(Printf.sprintf "%s" n)
  | Varray arr ->
    let arrLen = Array.length arr in
    if arrLen == 0 then Vstring "[]"
    else 
      let elements = Array.mapi (fun i v ->
        let str = match to_string v with
          | Vstring s -> s
          | _ -> failwith "Expected string in to_string"
        in
        if i == arrLen - 1 then str else str ^ ", "
      ) arr in
      Vstring ("[" ^ Array.fold_left (^) "" elements ^ "]")

let rec print_value e = 
  let v1 = to_string e in
  match v1 with
  | Vstring n -> Printf.printf "%s" n
  | _ -> failwith "Unsupported print"


  (* let is_false = function
  | Vnone
  | Vbool false
  | Vstring "" 
  | Vlist [||] -> true
  | Vnum n -> n = 0
  | _ -> false 

let is_true v = not (is_false v)  *)



  
let rec expr ctx = function 
  | Ecst (Cnone) -> Vnone
  | Ecst (Cnum n) -> Vnum n
  | Ecst (Cbool n) -> Vbool n
  | Ecst (Cstring n) -> Vstring n
  | Earray l -> 
    let arr = Array.of_list (List.map (expr ctx) l) in
    Varray arr
  | Eget (e1, e2) -> 
    let v1 = expr ctx e1 in
    let v2 = expr ctx e2 in
    begin match v1, v2 with
      | Varray arr, Vnum index -> 
        if index < 0.0 || index >= float (Array.length arr) then failwith "Index out of bounds"
        else arr.(int_of_float index)
      | _ -> failwith "Invalid array access"
    end
  | Elength e1 -> 
    let v1 = expr ctx e1 in
    begin match v1 with
      | Varray arr -> Vnum (float (Array.length arr))
      | _ -> failwith "Invalid length operation"
    end
  | Ebinop (Badd | Bsub | Bmul | Bdiv | Blt | Beq | Bgt | Bge | Ble | Bneq | Band | Bor  as op, e1, e2) ->
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      begin match op, v1, v2 with
        | Badd, Vnum n1, Vnum n2 -> Vnum (n1 +. n2)
        | Badd, Vstring n1, Vstring n2  -> Vstring (n1 ^ n2)
        | Badd, Varray n1, Varray n2 -> Varray (Array.append n1 n2)
        | Badd, _, _ -> 
            let s1 = match to_string v1 with Vstring s -> s | _ -> failwith "Expected string" in
            let s2 = match to_string v2 with Vstring s -> s | _ -> failwith "Expected string" in
            Vstring(s1 ^ s2)
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
    | Unot, Vbool n2 -> Vbool (not n2)
    | _ -> failwith "Unsupported Expression"
    end
    (* When we have an identity we find it in the hastable and return it. *)
  | Eident {id} -> Hashtbl.find ctx id
  | _ -> failwith "Unsupported expression"

(* stmts is all the statements in the block. *)
and stmt ctx = function
  | Sprint e -> print_value (expr ctx e)
  | Sblock stmts -> block ctx stmts
  | Sif (e, bl1, bl2) -> 
    let e1 = expr ctx e in
    begin match e1 with
      | Vbool e1 -> if e1 then stmt ctx bl1 else stmt ctx bl2 
      | _ -> failwith "Not boolean"
    end
  | Selseif (e, bl1, bl2) ->
    let e1 = expr ctx e in
    begin match e1 with
      | Vbool e1 -> if e1 then stmt ctx bl1  else stmt ctx bl2
      | _ -> failwith "Not boolean"
    end
  | Sassign ({id}, e1) ->
    Hashtbl.replace ctx id (expr ctx e1)
  | Sset (e1, e2, e3) ->
    let v1 = expr ctx e1 in
    let v2 = expr ctx e2 in
    let v3 = expr ctx e3 in
    begin match v1, v2 with
      | Varray arr, Vnum index -> 
        if index < 0.0 || index >= float (Array.length arr) then failwith "Index out of bounds"
        else arr.(int_of_float index) <- v3
      | _ -> failwith "Invalid array access"
    end
  
  | Sfor ({id}, e1, e2, s, bl) ->
    let v1 = expr ctx e1 in
    begin match v1 with
    | Vnum v1 ->
        (* Initialize variable in context *)
        Hashtbl.replace ctx id (Vnum v1);
        while
          (* Evaluate the condition *)
          match expr ctx e2 with
          | Vbool cond -> cond
          | _ -> failwith "For-loop condition must evaluate to a boolean"
        do
          (* Execute the block *)
          stmt ctx bl;
          (* Evaluate the stamtent in the end of for *)
          stmt ctx s
        done
    | _ -> failwith "For-loop start value must be a number"
    end
  
  | Srange (e1, e2, bl) ->
    let v1 = expr ctx e1 in
    let v2 = expr ctx e2 in
    begin match v1, v2 with
    | Vnum v1, Vnum v2 ->
      for _ = int_of_float v1 to int_of_float v2 do
        (* Execute the block *)
        stmt ctx bl
      done
    | _ -> failwith "For-loop start and end values must be numbers"
    end
  | Swhile (e, bl) ->
     while 
      (* This is done to evaluate the new value of the condition *)
       match expr ctx e with
        | Vbool cond -> cond
        | _ -> failwith "While-loop condition must evaluate to a boolean"
       do stmt ctx bl done
    (* Last case fail *)
  | _ -> failwith "Unsupported statement"
and block ctx = function
    | [] -> ()
    | s :: sl -> stmt ctx s; block ctx sl


let file (dl) =
  stmt (Hashtbl.create 17) dl

