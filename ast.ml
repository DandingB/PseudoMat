
   type location = Lexing.position * Lexing.position

   type ident = { loc: location; id: string; }
   
   type unop =
     | Uneg (* -e *)
     | Unot (* not e *)
   
     (* TODO: Remove unused operators. *)
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
     | Ecall of ident * expr list (* f(e1,e2,...)*)
     | Earray of expr list (* [e1,e2,...] *)
     | Eget of expr * expr (* e1[e2] *)
     | Ematrix of expr list list (* [[e1,e2,...],[e3,e4,...],...] *)
     | Elength of expr


   and stmt =
    | Sif of expr * stmt * stmt option  (* if (e1) {block1} else { block2 }, where block2 is optional *)
    | Selseif of expr * stmt * stmt option  (* elseif ( e1 ) { block1 } else { block2 }, where block2 is optional *)
    | Sreturn of expr
    | Sassign of ident * expr
    | Sprint of expr
    | Sblock of stmt list
    | Sfor of ident * expr * expr * stmt * stmt (* for(ident = e1; expr; stmt) {block}  *) (* For(i = 1; i<10; i++) {block}*)
    | Swhile of expr * stmt
    | Srange of expr * expr * stmt (* for(e1 to e2) {block} *)
    | Seval of expr
    | Sset of expr * expr * expr (* e1[e2] = e3 *)
    | Ssetmatrix of expr * expr * expr * expr (* e1[e2][e3] = e4 *)
    | Sfunc of ident * ident list * stmt (* function f(ident1, ident2, ...) {block} *)
   
   and func = ident * ident list * stmt
   
   and file = stmt
   