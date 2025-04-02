{
    open Parser

    let string_buffer = Buffer.create 1024

    (* Lexical analysis error *)
    exception Lexing_error of string
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let space = ' ' | '\t' | '\n'
let data_type = "number" | "string" | "boolean" | "array"

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
    | '=' { ASSIGN }
    | "!=" { NOTEQUALS }
    | '{' { LC }
    | '}' { RC }
    | '[' { LSQ }
    | ']' { RSQ }
    | ',' { COMMA }
    | "Not" { NOT }
    | "And" { AND }
    | "Or" { OR }
    | "True" { TRUE }
    | "False" { FALSE }
    | "If" { IF }
    | "Else" space+ "If" { ELSEIF }
    | "Else" { ELSE }
    | ';' { SEMICOLON }

    (* Comments *)
    | '#' [^'\n']* { next_token lexbuf }
    | "(*" { comment lexbuf }
    | digit+ as n { NUMBER (float_of_string n) }
    | digit+'.'digit+ as n { NUMBER (float_of_string n) }

    (* Variable decleration *)
    | "Let" { LET }
    | "as" { AS }
    | "be" { BE }

    (* Loops *)
    | "For" { FOR }
    | "to" { TO }
    | "While" { WHILE }
    (* Data types.  *)
    (* TODO: NOT IMPLEMENTED YET *)
    | data_type { DATATYPE }
    | letter (letter | digit | '_')* as id { ID id }
    | '"' { STRING(string lexbuf) }
    | eof { EOF }
    | _ as c { raise (Lexing_error ("Unexpected character: " ^ String.make 1 c)) }

and string = parse 
    | '"' { let s = Buffer.contents string_buffer in 
            Buffer.reset string_buffer ;
            s }
    | "\\\"" { Buffer.add_char string_buffer '"' ;
               string lexbuf }
    | "\\n" { Buffer.add_char string_buffer '\n' ;
               string lexbuf }
    | _ as c { Buffer.add_char string_buffer c ; 
               string lexbuf }
and comment = parse
    | "*)" { next_token lexbuf }
    | _ { comment lexbuf }
    | eof { raise (Lexing_error "Unterminated comment") }
    