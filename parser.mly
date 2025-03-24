%{
    open Ast
%}

%token ADD MUL DIV SUB
%token EOF
%token <int> NUMBER
%start <Ast.expr> main
%left ADD SUB (* Precedence *)
%left MUL DIV
%%

main:
 | e = expr EOF { e }
expr:
 | i = NUMBER { Ecst (Cint i) }
 | e1 = expr ADD e2 = expr { Ebinop (Badd, e1, e2) }
 | e1 = expr MUL e2 = expr { Ebinop (Bmul, e1, e2) }
 | e1 = expr DIV e2 = expr { Ebinop (Bdiv, e1, e2) }
 | e1 = expr SUB e2 = expr { Ebinop (Bsub, e1, e2) }