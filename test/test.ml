open Lib
open Parser
open Ast
open Testables

(* Helper functions *)
let get_token_list str =
  let lexbuf = Lexing.from_string str in
  let rec work acc =
    match Lexer.next_token lexbuf with
    | EOF -> acc
    | t -> work (t::acc)
  in List.rev (work[])

let token_stream tokens =
  let tokens = ref tokens in
  fun _lexbuf ->
    match !tokens with
    | [] -> Parser.EOF
    | tok :: rest ->
        tokens := rest;
        tok

(* Lexer unit tests *)
let test_unit_lexer_1 () =
  let str = "Let h be \"hej\" as string" in
  let expected = [LET; ID("h"); BE; STRING("hej"); AS; DATATYPE("string")] in
  let actual = get_token_list str in
  Alcotest.(check (list testable_token)) "Equal token lists" expected actual

let test_unit_lexer_2 () =
  let str = "Print(\"hej\" +  (3*3))" in
  let expected = [PRINT; LP; STRING("hej"); ADD; LP; NUMBER(3.); MUL; NUMBER(3.); RP; RP] in
  let actual = get_token_list str in
  Alcotest.(check (list testable_token)) "Equal token lists" expected actual

let test_unit_lexer_3 () =
  let str = "callToFunc(i*3)" in
  let expected = [ID("callToFunc"); LP; ID("i"); MUL; NUMBER(3.); RP] in
  let actual = get_token_list str in
  Alcotest.(check (list testable_token)) "Equal token lists" expected actual


let test_unit_lexer_4 () =
  let str = 
    "# Function that adds two numbers\n
    Function Add(a, b) {\n
    Let result be a + b as number\n
    Return result\n
    }\n\n
    Let x be Add(5, 3) as number" in

  let expected = 
    [FUNCTION; ID("Add"); LP; ID("a"); COMMA; ID("b"); RP; LC; 
    LET; ID("result"); BE; ID("a"); ADD; ID("b"); AS; DATATYPE("number"); 
    RETURN; ID("result"); 
    RC; 
    LET; ID("x"); BE; ID("Add"); LP; NUMBER(5.); COMMA; NUMBER(3.); RP; AS; DATATYPE("number")] in

  let actual = get_token_list str in
  Alcotest.(check (list testable_token)) "Equal token lists" expected actual


let test_unit_lexer_5 () =
  let str = 
    "Let arr1 be [1, 2, 3] as array\n
    arr1[0] = 5\n
    test = arr[2]" in

  let expected = 
    [LET; ID("arr1"); BE; LSQ; NUMBER(1.); COMMA;
    NUMBER(2.); COMMA; NUMBER(3.); RSQ; AS;
    DATATYPE("array"); ID("arr1"); LSQ; NUMBER(0.); 
    RSQ; ASSIGN; NUMBER(5.); ID("test"); ASSIGN; ID("arr");
    LSQ; NUMBER(2.); RSQ] in

  let actual = get_token_list str in
  Alcotest.(check (list testable_token)) "Equal token lists" expected actual


let test_unit_lexer_6 () =
  let str = 
    "Let c as 2x2 matrix\n
    c = [ \n
        1, 2; \n
        1, 2 \n
        ] \n
    \n
    Let b be [ \n
        1, 2; \n
        1, 2 \n
        ] as matrix \n
    \n
    c[0, 1] = 3" in

  let expected = 
    [LET; ID("c"); AS; DIMENSION(2., 2.);
    DATATYPE("matrix"); ID("c"); ASSIGN; LSQ; NUMBER(1.);
    COMMA; NUMBER(2.); SEMICOLON; NUMBER(1.); 
    COMMA; NUMBER(2.); RSQ; LET; ID("b"); BE; LSQ;
    NUMBER(1.); COMMA; NUMBER(2.); SEMICOLON;
    NUMBER(1.); COMMA; NUMBER(2.); RSQ; AS;
    DATATYPE("matrix"); ID("c"); LSQ; NUMBER(0.); COMMA;
    NUMBER(1.); RSQ; ASSIGN; NUMBER(3.)] in
    
  let actual = get_token_list str in
  Alcotest.(check (list testable_token)) "Equal token lists" expected actual



(* Parser unit tests *)
let test_unit_parser_1 () =
  let tokens = [PRINT; LP; STRING("hej"); ADD; LP; NUMBER(3.); MUL; NUMBER(3.); RP; RP] in
  let expected = 
    Sblock [
      Sprint (
        Ebinop (
          Badd, 
          Ecst ( Cstring ("hej") ), 
          Ebinop (
            Bmul, 
            Ecst (Cnum (3.)), 
            Ecst (Cnum (3.))
          )
        )
      ); 
    ] in

  let lexbuf = Lexing.from_string "" in (* Required for position tracking *)
  let actual = Parser.main (token_stream tokens) lexbuf in
  Alcotest.(check testable_ast) "AST should match" expected actual

let test_unit_parser_2 () =
  let tokens = 
    [LET; ID("myString"); BE; STRING("hello"); AS; DATATYPE("string");
    LET; ID("testing"); BE; NUMBER(2.); AS; DATATYPE("number")] in
  
  let expected = 
    Sblock [
      Sassign (
        {id = "myString"}, 
        Ecst (Cstring ("hello")), 
        "string"
      ); 
      Sassign (
        {id = "testing"}, 
        Ecst (Cnum (2.)), 
        "number"
      ); 
    ] in

  let lexbuf = Lexing.from_string "" in (* Required for position tracking *)
  let actual = Parser.main (token_stream tokens) lexbuf in
  Alcotest.(check testable_ast) "AST should match" expected actual

let test_unit_parser_3 () =
  let tokens = 
    [FUNCTION; ID("Add"); LP; ID("a"); COMMA; ID("b"); RP; LC; 
    LET; ID("result"); BE; ID("a"); ADD; ID("b"); AS; DATATYPE("number"); 
    RETURN; ID("result"); 
    RC; 
    LET; ID("x"); BE; ID("Add"); LP; NUMBER(5.); COMMA; NUMBER(3.); RP; AS; DATATYPE("number")] in

  let expected = 
    Sblock [
      Sfunc (
        {id="Add"}, 
        [{id="a"}; {id="b"}], 
        Sblock ([
          Sassign (
            {id="result"}, 
            Ebinop (Badd, Eident ({id="a"}), Eident ({id="b"})), 
            "number"
          ); 
          Sreturn (Eident ({id="result"})); 
        ])
      ); 
      Sassign (
        {id="x"}, 
        Ecall (
          {id="Add"}, 
          [
            Ecst (Cnum (5.));
            Ecst (Cnum (3.));
          ]
        ), 
        "number"
      ); 
    ] in

  let lexbuf = Lexing.from_string "" in (* Required for position tracking *)
  let actual = Parser.main (token_stream tokens) lexbuf in
  Alcotest.(check testable_ast) "AST should match" expected actual
  
let test_unit_parser_4 () =
  let tokens = 
    [LET; ID("arr1"); BE; LSQ; NUMBER(1.); COMMA;
    NUMBER(2.); COMMA; NUMBER(3.); RSQ; AS;
    DATATYPE("array"); ID("arr1"); LSQ; NUMBER(0.); 
    RSQ; ASSIGN; NUMBER(5.); ID("test"); ASSIGN; ID("arr");
    LSQ; NUMBER(2.); RSQ] in
  
  let expected = 
    Sblock [
      Sassign (
        {id="arr1"}, 
        Earray ([
          Ecst (Cnum (1.000000));
          Ecst (Cnum (2.000000));
          Ecst (Cnum (3.000000));
        ]), 
        "array"
      ); 
      Sset (
        Eident ({id="arr1"}), 
        Ecst (Cnum (0.000000)), 
        Ecst (Cnum (5.000000))
      ); 
      Sassign (
        {id="test"}, 
        Eget (
          Eident ({id="arr"}), 
          Ecst (Cnum (2.000000))
        ),
        ""
      ); 
    ] in

  let lexbuf = Lexing.from_string "" in (* Required for position tracking *)
  let actual = Parser.main (token_stream tokens) lexbuf in
  Alcotest.(check testable_ast) "AST should match" expected actual


let () =
  let open Alcotest in
  run "Unit" [
    "Lexer", [
      test_case "Declaration" `Quick test_unit_lexer_1;
      test_case "Print + some expression" `Quick test_unit_lexer_2;
      test_case "Call to function" `Quick test_unit_lexer_3;
      test_case "Decl of function and call" `Quick test_unit_lexer_4;
      test_case "Arrays" `Quick test_unit_lexer_5;
      test_case "Matrices" `Quick test_unit_lexer_6;
    ];

    "Parser", [
      test_case "Simple expression" `Quick test_unit_parser_1;
      test_case "Declaration" `Quick test_unit_parser_2;
      test_case "Function declaration and call" `Quick test_unit_parser_3;
      test_case "Arrays" `Quick test_unit_parser_4;
    ];
  ]

