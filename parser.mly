%{
    open Ast
%}

%token ADD MUL DIV SUB
%token EOF
%token <float> NUMBER
%token <string> STRING
%token TRUE FALSE 
%token IF ELSE
%token PRINT
%token LP RP LC RC
%token NEWLINE
%token SEMICOLON
%token LESS GREATER LESSEQUALS GREATEREQUALS EQUALS NOTEQUALS AND OR
%start <Ast.stmt> main
%left ADD SUB (* Precedence *)
%left MUL DIV
%nonassoc USUB
%%

main:
 | NEWLINE? e = nonempty_list(block) NEWLINE? EOF { Sblock e }

block:
 | NEWLINE? e1 = stmt NEWLINE? { e1 }
 | NEWLINE? LC NEWLINE? e = nonempty_list(stmt) NEWLINE? RC { Sblock e }

stmt:
 | PRINT LP e = expr RP { Sprint e }
 | IF LP e = expr RP b = block { Sif (e, b, Sblock []) }
 | IF LP e = expr RP b1 = block ELSE b2 = block { Sif (e, b1, b2) }
 | e = expr
    { Seval e }

expr:
 | i = NUMBER { Ecst (Cnum i) }
 | s = STRING { Ecst (Cstring s) }
 | TRUE { Ecst (Cbool true)}
 | FALSE { Ecst (Cbool false) }
 | e1 = expr ADD e2 = expr { Ebinop (Badd, e1, e2) }
 | e1 = expr MUL e2 = expr { Ebinop (Bmul, e1, e2) }
 | e1 = expr DIV e2 = expr { Ebinop (Bdiv, e1, e2) }
 | e1 = expr SUB e2 = expr { Ebinop (Bsub, e1, e2) }
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



