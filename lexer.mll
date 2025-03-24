

{
    open Parser

}
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = parse 
    | [' ' '\t' '\n'] { token lexbuf }
    | '+' { ADD }
    | '-' { SUB }
    | '*' {MUL}
    | '/' { DIV }
    | digit+ as n { NUMBER (int_of_string n) }¨
    | eof { EOF }
{

}

