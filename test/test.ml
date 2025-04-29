open Ast
open Interp

let assert_equal expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "Assertion failed:\nExpected: %s\nActual:   %s" expected actual)

let reset_output () =
  Buffer.clear test_output

let () =
  Interp.is_test_mode := true;

  (* Addition *)
  reset_output ();
  file (Sprint (Ebinop (Badd, Ecst (Cnum 2.0), Ecst (Cnum 3.0))));
  assert_equal "5" (Buffer.contents test_output);

  (* String concatenation *)
  reset_output ();
  file (Sprint (Ebinop (Badd, Ecst (Cstring "Hello "), Ecst (Cstring "World!"))));
  assert_equal "Hello World!" (Buffer.contents test_output);

  (* Array printing *)
  reset_output ();
  file (Sprint (Earray [Ecst (Cnum 1.0); Ecst (Cnum 2.0); Ecst (Cnum 3.0)]));
  assert_equal "[1, 2, 3]" (Buffer.contents test_output);

  (* Matrix addition *)
  reset_output ();
  file (Sprint (Ebinop (Badd,
            Ematrix [[Ecst (Cnum 1.0); Ecst (Cnum 2.0)]],
            Ematrix [[Ecst (Cnum 3.0); Ecst (Cnum 4.0)]]
         )));
  assert_equal "[[4, 6]]" (Buffer.contents test_output);

  (* Function call *)
  reset_output ();
  file (Sblock [
    Sfunc ({loc=(Lexing.dummy_pos, Lexing.dummy_pos); id="Add"},
           [{loc=(Lexing.dummy_pos, Lexing.dummy_pos); id="a"};
            {loc=(Lexing.dummy_pos, Lexing.dummy_pos); id="b"}],
           Sreturn (Ebinop (Badd, Eident {loc=(Lexing.dummy_pos, Lexing.dummy_pos); id="a"},
                                Eident {loc=(Lexing.dummy_pos, Lexing.dummy_pos); id="b"}))
    );
    Sprint (Ecall ({loc=(Lexing.dummy_pos, Lexing.dummy_pos); id="Add"},
                   [Ecst (Cnum 10.0); Ecst (Cnum 5.0)]))
  ]);
  assert_equal "15" (Buffer.contents test_output);

  print_endline "✅ All tests passed!"
