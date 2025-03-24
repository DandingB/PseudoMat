{
    open Parser
}
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let space = ' ' | '\t'

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
    | digit+ as n { NUMBER (int_of_string n) }
    | eof { EOF }
