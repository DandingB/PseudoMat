%{
    open Ast
%}

%token ADD MUL DIV SUB
%token EOF
%token <int> NUMBER
%token PRINT
%token LP RP
%token NEWLINE

%start <Ast.stmt> main
%left ADD SUB (* Precedence *)
%left MUL DIV
%%

main:
 | e = stmt EOF { e }
expr:
 | i = NUMBER { Ecst (Cint i) }
 | e1 = expr ADD e2 = expr { Ebinop (Badd, e1, e2) }
 | e1 = expr MUL e2 = expr { Ebinop (Bmul, e1, e2) }
 | e1 = expr DIV e2 = expr { Ebinop (Bdiv, e1, e2) }
 | e1 = expr SUB e2 = expr { Ebinop (Bsub, e1, e2) }

stmt:
 | e1 = stmt NEWLINE e2 = stmt { Sblock[e1; e2] }
 | PRINT LP e = expr RP { Sprint e }
