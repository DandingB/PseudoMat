open Lib
open Parser

let pp_token fmt p = 
  match p with
  | WHILE -> Format.fprintf fmt "WHILE"
  | TRUE -> Format.fprintf fmt "TRUE"
  | TO -> Format.fprintf fmt "TO"
  | SUB -> Format.fprintf fmt "SUB"
  | STRING s -> Format.fprintf fmt "STRING(\"%s\")" s
  | SEMICOLON -> Format.fprintf fmt "SEMICOLON"
  | RSQ -> Format.fprintf fmt "RSQ"
  | RP -> Format.fprintf fmt "RP"
  | RETURN -> Format.fprintf fmt "RETURN"
  | RC -> Format.fprintf fmt "RC"
  | PRINT -> Format.fprintf fmt "PRINT"
  | POW -> Format.fprintf fmt "POW"
  | OR -> Format.fprintf fmt "OR"
  | NUMBER f -> Format.fprintf fmt "NUMBER(%f)" f
  | NOTEQUALS -> Format.fprintf fmt "NOTEQUALS"
  | NOT -> Format.fprintf fmt "NOT"
  | MUL -> Format.fprintf fmt "MUL"
  | MOD -> Format.fprintf fmt "MOD"
  | LSQ -> Format.fprintf fmt "LSQ"
  | LP -> Format.fprintf fmt "LP"
  | LET -> Format.fprintf fmt "LET"
  | LESSEQUALS -> Format.fprintf fmt "LESSEQUALS"
  | LESS -> Format.fprintf fmt "LESS"
  | LENGTH -> Format.fprintf fmt "LENGTH"
  | LC -> Format.fprintf fmt "LC"
  | IF -> Format.fprintf fmt "IF"
  | ID s -> Format.fprintf fmt "ID('%s')" s
  | GREATEREQUALS -> Format.fprintf fmt "GREATEREQUALS"
  | GREATER -> Format.fprintf fmt "GREATER"
  | FUNCTION -> Format.fprintf fmt "FUNCTION"
  | FOR -> Format.fprintf fmt "FOR"
  | FALSE -> Format.fprintf fmt "FALSE"
  | EQUALS -> Format.fprintf fmt "EQUALS"
  | EOF -> Format.fprintf fmt "EOF"
  | ELSEIF -> Format.fprintf fmt "ELSEIF"
  | ELSE -> Format.fprintf fmt "ELSE"
  | DOT -> Format.fprintf fmt "DOT"
  | DIV -> Format.fprintf fmt "DIV"
  | DIMENSION (f1, f2) -> Format.fprintf fmt "DIMENSION(%f,%f)" f1 f2
  | DATATYPE s -> Format.fprintf fmt "DATATYPE('%s')" s
  | COMMA -> Format.fprintf fmt "COMMA"
  | BE -> Format.fprintf fmt "BE"
  | ASSIGN -> Format.fprintf fmt "ASSIGN"
  | AS -> Format.fprintf fmt "AS"
  | AND -> Format.fprintf fmt "AND"
  | ADD -> Format.fprintf fmt "ADD"

let equal_token p1 p2 = 
  if p1 = p2 then true else false

let testable_token = Alcotest.testable pp_token equal_token



let get_token_list str =
  let lexbuf = Lexing.from_string str in
  let rec work acc =
    match Lexer.next_token lexbuf with
    | EOF -> acc
    | t -> work (t::acc)
  in List.rev (work[])

let test_unit_lexer_1 () =
  let str = "Let h be \"hej\" as string" in
  let expected = [LET; ID("h"); BE; STRING("hej"); AS; DATATYPE("string")] in
  let actual = get_token_list str in
  Alcotest.(check (list testable_token)) "same lists" expected actual

let test_unit_lexer_2 () =
  let str = "Print(\"hej\" +  (3*3))" in
  let expected = [PRINT; LP; STRING("hej"); ADD; LP; NUMBER(3.); MUL; NUMBER(3.); RP; RP] in
  let actual = get_token_list str in
  Alcotest.(check (list testable_token)) "same lists" expected actual


let () =
  let open Alcotest in
  run "lexer" [
    "unit-lexer", [
      test_case "Declaration" `Quick test_unit_lexer_1;
      test_case "Print + some expression" `Quick test_unit_lexer_2;
    ]
  ]