%{
    open Ast
%}

// TODO: Make comments about tokens.
%token ADD MUL DIV SUB
%token EOF
%token <float> NUMBER
%token <string> STRING
%token TRUE FALSE 
%token IF ELSEIF ELSE
%token PRINT
%token LP RP LC RC
%token NEWLINE
%token SEMICOLON
%token LESS GREATER LESSEQUALS GREATEREQUALS EQUALS NOTEQUALS AND OR
%token LET AS BE ASSIGN DATATYPE
%token <string> ID
%start <Ast.stmt> main
%left ADD SUB (* Precedence *)
%left MUL DIV
%nonassoc USUB
%%

main:
 | NEWLINE? e = nonempty_list(block) NEWLINE? EOF { Sblock e }

block:
 | NEWLINE? e1 = stmt NEWLINE? { e1 }
 | NEWLINE? LC NEWLINE? e = nonempty_list(stmt) NEWLINE? RC NEWLINE? { Sblock e }

stmt:
 | PRINT LP e = expr RP { Sprint e }
//  If statement without else
 | IF LP e = expr RP b = block { Sif (e, b, Sblock []) }
//  If statement with else
 | IF LP e = expr RP b1 = block ELSE b2 = block { Sif (e, b1, b2) }
//  If statement with elseif. We check if there has been an if before the else if.
 | IF LP expr RP block ELSEIF LP e = expr RP b1 = block { Selseif (e, b1, Sblock []) }
//  If, elseif and else match.
 | IF LP expr RP block ELSEIF LP e = expr RP b1 = block ELSE b2 = block { Selseif (e, b1, b2) }
//  Else if statement. We check if there has been an else if before the else if.
 | ELSEIF LP expr RP block ELSEIF LP e = expr RP b1 = block { Selseif (e, b1, Sblock []) }
//  Else after elseif. We check if there has been an else if before the else.
 | ELSEIF LP e = expr RP b1 = block ELSE b2 = block { Selseif (e, b1, b2) }

//  First expression is the ID expression returning Eident. Second is the value of the ID expression.
// This will match: Let id as type be value
 | NEWLINE? LET e1 = ident BE e2 = expr AS DATATYPE NEWLINE? { Sassign (e1, e2) } 
 | NEWLINE? LET e1 = ident AS DATATYPE NEWLINE? { Sassign (e1, Ecst(Cnone) ) } 
//  Assign new value to variabble. This will match: id = value
 | e1 = ident ASSIGN e2 = expr NEWLINE? { Sassign (e1, e2) }
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



ident:
  id = ID { { loc = ($startpos, $endpos); id } }
;