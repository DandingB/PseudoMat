{
    open AST
}

%token ADD MUL DIV SUB
%token EOF
%token <int> NUMBER 
%start main
%type <int> main
%%

main:
 | e = expr EOF { e }

expr: 
 | i = NUMBER { i }
 | e1 = expr ADD e2 = expr { e1 + e2 }
 | e1 = expr MUL e2 = expr { e1 * e2 }
 | e1 = expr DIV e2 = expr { e1 / e2 }
 | e1 = expr SUB e2 = expr { e1 - e2 }






