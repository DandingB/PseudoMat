{
    open Parser
}
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = parse 
    | [' ' '\t'] { token lexbuf }
    | '\n' { NEWLINE }
    | '+' { ADD }
    | '-' { SUB }
    | '*' {MUL}
    | '/' { DIV }
    | "Print" { PRINT }
    | '(' { LP }
    | ')' { RP }
    | digit+ as n { NUMBER (int_of_string n) }
    | eof { EOF }
{

}

