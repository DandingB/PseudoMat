
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
     | Sif of expr * stmt * stmt (* if e1 then s1 else s2 *)
     | Selseif of expr * stmt * stmt (* elseif e1 then s1 else s2 *)
     | Sreturn of expr
     | Sassign of ident * expr
     | Sprint of expr
     | Sblock of stmt list
     | Sfor of ident * expr * expr * stmt * stmt (* for(ident = e1; expr; stmt) {block}  *) (* For(i = 1; i<10; i++) {block}*)
     | Swhile of expr * stmt
     | Srange of expr * expr * stmt (* for(e1 to e2) {block} *)
     | Seval of expr
     | Sset of ident * expr * expr (* e1[e2] = e3 *)
     | Slength of ident
   
   and def = ident * ident list * stmt
   
   and file = def list * stmt
   