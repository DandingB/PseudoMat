%{
    open Ast
    open Helpers
%}

// TODO: Make comments about tokens.
%token ADD MUL DIV SUB POW MOD
%token NOT
%token EOF
%token <float> NUMBER
%token <string> STRING
%token <float * float> DIMENSION
%token TRUE FALSE 
%token IF ELSEIF ELSE
%token LENGTH
%token FOR TO WHILE
%token PRINT
%token LP RP LC RC LSQ RSQ
%token SEMICOLON COMMA DOT 
%token LESS GREATER LESSEQUALS GREATEREQUALS EQUALS NOTEQUALS AND OR
%token LET AS BE ASSIGN 
%token <string> DATATYPE
%token FUNCTION RETURN
%token <string> ID
%start <Ast.file> main
(* Precedence *)
%left AND OR 
%left LESS GREATER LESSEQUALS GREATEREQUALS EQUALS NOTEQUALS
%left ADD SUB 
%left MUL DIV MOD
%left POW
%nonassoc USUB UNOT
%%


main:
 | e = nonempty_list(block) EOF { Sblock e }

func: 
 | FUNCTION id = ident LP args = separated_list(COMMA, ident) RP b = block { id, args, b }

block:
 | e1 = stmt { e1 }
 | LC e = nonempty_list(stmt) RC { Sblock e }

// Else If blocks. Retuns a list of tuples consiting of  the expression and the block: [(expr1,block1), (expr2,block2),...].
elseif_blocks:
 { [] }
| ELSEIF LP e = expr RP b = block rest = elseif_blocks { (e, b) :: rest }

// Else block. Returns an optional block. If there is no else block, we return None. If there is an else block, we return Some b.
else_block:
 { None }
//   We retun the Some b. Some is a buildin constructor in OCaml. Some is used to wrap a value into the option type.
| ELSE b = block { Some b }

stmt:
  | PRINT LP e = expr RP { Sprint e }
  | IF LP e1 = expr RP b1 = block elseifs = elseif_blocks elseopt = else_block 
    { build_if_chain e1 b1 elseifs elseopt }
  //  First expression is the ID expression returning Eident. Second is the value of the ID expression.
  // This will match: Let id as type be value
  | LET e1 = ident BE e2 = expr AS dt = DATATYPE { 
        (* If the datatype is matrix and the expression is array, we assign as matrix type *)
        (* Else we just return the expression as is*)
        let value = match dt, e2 with
          | "matrix", Earray l -> Ematrix [l]
          | _, _ -> e2
        in
        Sassign (e1, value, dt)
      }
  | LET e1 = ident AS dt = DATATYPE { Sassign (e1, Ecst(Cnone), dt) } 
  | LET e1 = ident AS d = DIMENSION dt = DATATYPE
    {
      let (w, h) = d in
      let rows = int_of_float h in
      let cols = int_of_float w in
      let matrix = Ematrix (
        List.init rows (fun _ -> List.init cols (fun _ -> Ecst (Cnum 0.)))
      ) in
      Sassign (e1, matrix, dt)
    } 
  //  Assign new value to variabble. This will match: id = value
  | e1 = ident ASSIGN e2 = expr { Sassign (e1, e2, "") }
  | e1 = expr LSQ e2 = expr COMMA e3 = expr RSQ ASSIGN e4 = expr { Ssetmatrix (e1, e2, e3, e4) } (* Assign value to matrix. *)
  //  Assign value to array. 
  | e1 = expr LSQ e2 = expr RSQ ASSIGN e3 = expr { Sset (e1, e2, e3) }
  //  FOR LOOPS
  | FOR LP id = ident ASSIGN e1 = expr SEMICOLON e2 = expr SEMICOLON s = stmt RP b = block { Sfor (id, e1,e2,s,b) } (* for(id = e1; e2; s) {b} *)
  | FOR LP e1 = expr TO e2 = expr RP b = block {Srange(e1, e2, b) } (* for(e1 to e2) {b} *)
  //  WHILE LOOPS
  | WHILE LP e = expr RP b = block {Swhile(e, b) } (* for(e1 to e2) {b} *)
  //  Function
  | f = func { let (id, args, b) = f in Sfunc (id, args, b) } (* function definition *)
  | RETURN e = expr { Sreturn e } (* return e *)
  | e = expr
    { Seval e }

expr:
 | i = NUMBER { Ecst (Cnum i) }
 | s = STRING { Ecst (Cstring s) }
 | TRUE { Ecst (Cbool true)}
 | FALSE { Ecst (Cbool false) }
 | id = ident { Eident id }
 | e1 = expr ADD e2 = expr { Ebinop (Badd, e1, e2) }
 | e1 = expr MUL e2 = expr { Ebinop (Bmul, e1, e2) }
 | e1 = expr DIV e2 = expr { Ebinop (Bdiv, e1, e2) }
 | e1 = expr SUB e2 = expr { Ebinop (Bsub, e1, e2) }
 | e1 = expr POW e2 = expr { Ebinop (Bpow, e1, e2) }
 | e1 = expr MOD e2 = expr { Ebinop (Bmod, e1, e2) }
 | e1 = expr LESS e2 = expr { Ebinop(Blt, e1, e2) }
 | e1 = expr GREATER e2 = expr { Ebinop(Bgt, e1, e2) }
 | e1 = expr LESSEQUALS e2 = expr { Ebinop(Ble, e1, e2) }
 | e1 = expr GREATEREQUALS e2 = expr { Ebinop(Bge, e1, e2) }
 | e1 = expr EQUALS e2 = expr { Ebinop(Beq, e1, e2) }
 | e1 = expr NOTEQUALS e2 = expr { Ebinop(Bneq, e1, e2) }
 | e1 = expr AND e2 = expr { Ebinop(Band, e1, e2) }
 | e1 = expr OR e2 = expr { Ebinop(Bor, e1, e2) }
 | LP e = expr RP { e }
 | SUB e = expr %prec USUB { Eunop(Uneg, e) }
 | NOT e = expr %prec UNOT { Eunop(Unot, e) }
 | LSQ l = separated_list(COMMA, expr) RSQ { Earray l }
 | LSQ rows = matrix_rows RSQ { Ematrix rows }
 | e1 = expr LSQ e2 = expr RSQ { Eget (e1, e2) }
 | e1 = expr LSQ e2 = expr COMMA e3 = expr RSQ { Egetmatrix (e1, e2, e3) }
 | e1 = expr DOT LENGTH { Elength e1 }
//  Function call.
 | func_id = ident LP expr_list = separated_list(COMMA, expr) RP
    { Ecall (func_id, expr_list) }

// Modified matrix handling
matrix_rows:
 | row = matrix_row { [row] }
 | row = matrix_row SEMICOLON rest = matrix_rows { row :: rest }

matrix_row:
 | elements = separated_list(COMMA, expr) { elements }


ident:
  id = ID { { id } }
;