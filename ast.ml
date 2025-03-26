
   type location = Lexing.position * Lexing.position

   type ident = { loc: location; id: string; }
   
   type unop =
     | Uneg (* -e *)
     | Unot (* not e *)
   
   type binop =
     | Badd | Bsub | Bmul | Bdiv | Bmod | Bpow   (* + - * // % ^ *)
     | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
     | Band | Bor                          (* and or *)
   
    (* Simplest form of expression *)
   type constant =
     | Cnone
     | Cbool of bool
     | Cstring of string 
     | Cnum of float
   
     (* Expressions combination of constants or other expressions *)
   type expr =
     | Ecst of constant
     | Eident of ident
     | Ebinop of binop * expr * expr
     | Eunop of unop * expr
     | Ecall of ident * expr list
     | Earray of expr list (* [e1,e2,...] *)
     | Eget of expr * expr (* e1[e2] *)
     | Ematrix of expr list list (* [[e1,e2,...],[e3,e4,...],...] *)
   
   and stmt =
     | Sif of expr * stmt
     | Sreturn of expr
     | Sassign of ident * expr
     | Sprint of expr
     | Sblock of stmt list
     | Sfor of ident * expr * stmt
     | Seval of expr
     | Sset of expr * expr * expr (* e1[e2] = e3 *)
   
   and def = ident * ident list * stmt
   
   and file = def list * stmt
   