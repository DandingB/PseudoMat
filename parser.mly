
%token ADD MUL DIV SUB
%token EOF
%token <int> NUMBER
%start main
%type <int> main
%type <int> expr
%left ADD SUB (* Precedence *)
%left MUL DIV
%%

main:
 | e = expr EOF { e }
expr:
 | i = NUMBER { i }
 | e1 = expr ADD e2 = expr { e1 + e2 }
 | e1 = expr MUL e2 = expr { e1 * e2 }
 | e1 = expr DIV e2 = expr { e1 / e2 }
 | e1 = expr SUB e2 = expr { e1 - e2 }