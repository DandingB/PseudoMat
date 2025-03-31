{
    open Parser

    let string_buffer = Buffer.create 1024
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let space = ' ' | '\t' | '\n'

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
    | '{' { LC }
    | '}' { RC }
    | "And" { AND }
    | "Or" { OR }
    | "True" { TRUE }
    | "False" { FALSE }
    | "If" { IF }
    | "Else" space+ "If" { ELSEIF }
    | "Else" { ELSE }
    | digit+ as n { NUMBER (float_of_string n) }
    | digit+'.'digit+ as n { NUMBER (float_of_string n) }
    | '"' { STRING(string lexbuf) }
    | eof { EOF }

and string = parse 
    | '"' { let s = Buffer.contents string_buffer in 
            Buffer.reset string_buffer ;
            s }
    | "\\\"" { Buffer.add_char string_buffer '"' ;
               string lexbuf }
    | _ as c { Buffer.add_char string_buffer c ; 
               string lexbuf }
    