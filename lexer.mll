{
    open Parser
}
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let space = ' ' | '\t'
let string = letter | digit | space

rule next_token = parse 
    | space+ { next_token lexbuf }
    | '\n' { NEWLINE }
    | '+' { ADD }
    | '-' { SUB }
    | '*' {MUL}
    | '/' { DIV }
    | "Print" { PRINT }
    | '(' { LP }
    | ')' { RP }
    | '<' { LESS }
    | '>' { GREATER }
    | "<=" { LESSEQUALS }
    | ">=" { GREATEREQUALS }
    | "==" { EQUALS }
    | "!=" { NOTEQUALS }
    | "And" { AND }
    | "Or" { OR }
    | "True" { TRUE }
    | "False" { FALSE }
    | digit+ as n { NUMBER (float_of_string n) }
    | digit+'.'digit+ as n { NUMBER (float_of_string n) }
    | '"'string*'"' as n { STRING (n) }
    | eof { EOF }
{

}

