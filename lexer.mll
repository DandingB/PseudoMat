{
    open Parser

    let string_buffer = Buffer.create 1024

    (* Lexical analysis error *)
    exception Lexing_error of string
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let space = ' ' | '\t' | '\n' | '\r'
let data_type = "number" | "string" | "boolean" | "array" | "matrix"

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
    | '.' { DOT }
    | "Not" { NOT }
    | "And" { AND }
    | "Or" { OR }
    | "True" { TRUE }
    | "False" { FALSE }
    | "If" { IF }
    | "Else" space+ "If" { ELSEIF }
    | "Else" { ELSE }
    | ';' { SEMICOLON }
    | '^' { POW }
    | '%' { MOD }

    (* Function *)
    | "Function" { FUNCTION }
    | "Return" { RETURN }
    
    (* Array *)
    | "Length" { LENGTH }
    | (digit+ as n1) 'x' (digit+ as n2) { DIMENSION(float_of_string n1, float_of_string n2) }

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
    | data_type as dt { DATATYPE dt }
    | letter (letter | digit | '_')* as id { ID id }
    | '"' { STRING(string lexbuf) }
    | eof { EOF }
    | _ as c { 
        let pos = Lexing.lexeme_start_p lexbuf in
        let line = pos.Lexing.pos_lnum in
        let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
        raise (Lexing_error (Printf.sprintf "Unexpected character '%c' at line %d, column %d" c line col)
        )
}

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
    | eof { raise (Lexing_error "Unterminated string") }
and comment = parse
    | "*)" { next_token lexbuf }
    | _ { comment lexbuf }
    | eof { raise (Lexing_error "Unterminated comment") }
    