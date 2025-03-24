
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | SUB
    | NUMBER of (
# 4 "parser.mly"
       (int)
# 16 "parser.ml"
  )
    | MUL
    | EOF
    | DIV
    | ADD
  
end

include MenhirBasics

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_main) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState04 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 04.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState06 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 06.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState08 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 08.
        Stack shape : expr.
        Start symbol: main. *)

  | MenhirState10 : (('s, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 10.
        Stack shape : expr.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (
# 7 "parser.mly"
      (int)
# 58 "parser.ml"
)

and _menhir_box_main = 
  | MenhirBox_main of (
# 6 "parser.mly"
      (int)
# 65 "parser.ml"
) [@@unboxed]

let _menhir_action_1 =
  fun i ->
    (
# 12 "parser.mly"
              ( i )
# 73 "parser.ml"
     : (
# 7 "parser.mly"
      (int)
# 77 "parser.ml"
    ))

let _menhir_action_2 =
  fun e1 e2 ->
    (
# 13 "parser.mly"
                           ( e1 + e2 )
# 85 "parser.ml"
     : (
# 7 "parser.mly"
      (int)
# 89 "parser.ml"
    ))

let _menhir_action_3 =
  fun e1 e2 ->
    (
# 14 "parser.mly"
                           ( e1 * e2 )
# 97 "parser.ml"
     : (
# 7 "parser.mly"
      (int)
# 101 "parser.ml"
    ))

let _menhir_action_4 =
  fun e1 e2 ->
    (
# 15 "parser.mly"
                           ( e1 / e2 )
# 109 "parser.ml"
     : (
# 7 "parser.mly"
      (int)
# 113 "parser.ml"
    ))

let _menhir_action_5 =
  fun e1 e2 ->
    (
# 16 "parser.mly"
                           ( e1 - e2 )
# 121 "parser.ml"
     : (
# 7 "parser.mly"
      (int)
# 125 "parser.ml"
    ))

let _menhir_action_6 =
  fun e ->
    (
# 10 "parser.mly"
                ( e )
# 133 "parser.ml"
     : (
# 6 "parser.mly"
      (int)
# 137 "parser.ml"
    ))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ADD ->
        "ADD"
    | DIV ->
        "DIV"
    | EOF ->
        "EOF"
    | MUL ->
        "MUL"
    | NUMBER _ ->
        "NUMBER"
    | SUB ->
        "SUB"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let i = _v in
      let _v = _menhir_action_1 i in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState10 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState08 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState06 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState04 ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_11 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_2 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState04 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | NUMBER _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_06 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState06 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | NUMBER _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState08 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | NUMBER _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_10 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState10 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | NUMBER _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_4 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_3 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_5 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF ->
          let e = _v in
          let _v = _menhir_action_6 e in
          MenhirBox_main _v
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | NUMBER _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
