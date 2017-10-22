
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | STRING of (
# 1 "src/parser.mly"
       (string)
# 11 "src/parser.ml"
  )
    | RIGHT_PARENTHESIS
    | RIGHT_BRACKET
    | RIGHT_BRACE
    | RIGHT_ANGLE_BRACKET
    | LEFT_PARENTHESIS
    | LEFT_BRACKET
    | LEFT_BRACE
    | LEFT_ANGLE_BRACKET
    | HOLE of (
# 2 "src/parser.mly"
       (string)
# 24 "src/parser.ml"
  )
    | EOF
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState20
  | MenhirState8
  | MenhirState6
  | MenhirState4
  | MenhirState2
  | MenhirState0

let rec _menhir_goto_fragment_values : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_fragment_values -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv50 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 10:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_ANGLE_BRACKET ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHT_ANGLE_BRACKET) to state 11\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv46 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
            ((Printf.fprintf Pervasives.stderr "State 11:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv44 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
            ((Printf.fprintf Pervasives.stderr "Reducing production fragment_values -> LEFT_ANGLE_BRACKET fragment_values RIGHT_ANGLE_BRACKET \n%!";
            let ((_menhir_stack, _menhir_s), _, (f : 'tv_fragment_values)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_fragment_values = 
# 28 "src/parser.mly"
( [`Delimiter "<"] @ f @ [`Delimiter ">"] )
# 76 "src/parser.ml"
             in
            _menhir_goto_fragment_values _menhir_env _menhir_stack _menhir_s _v) : 'freshtv45)) : 'freshtv47)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv48 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv49)) : 'freshtv51)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv58 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 12:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACE ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHT_BRACE) to state 13\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv54 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
            ((Printf.fprintf Pervasives.stderr "State 13:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv52 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
            ((Printf.fprintf Pervasives.stderr "Reducing production fragment_values -> LEFT_BRACE fragment_values RIGHT_BRACE \n%!";
            let ((_menhir_stack, _menhir_s), _, (f : 'tv_fragment_values)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_fragment_values = 
# 24 "src/parser.mly"
( [`Delimiter "{"] @ f @ [`Delimiter "}"] )
# 109 "src/parser.ml"
             in
            _menhir_goto_fragment_values _menhir_env _menhir_stack _menhir_s _v) : 'freshtv53)) : 'freshtv55)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv56 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv57)) : 'freshtv59)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv66 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 14:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACKET ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHT_BRACKET) to state 15\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv62 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
            ((Printf.fprintf Pervasives.stderr "State 15:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv60 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
            ((Printf.fprintf Pervasives.stderr "Reducing production fragment_values -> LEFT_BRACKET fragment_values RIGHT_BRACKET \n%!";
            let ((_menhir_stack, _menhir_s), _, (f : 'tv_fragment_values)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_fragment_values = 
# 26 "src/parser.mly"
( [`Delimiter "["] @ f @ [`Delimiter "]"] )
# 142 "src/parser.ml"
             in
            _menhir_goto_fragment_values _menhir_env _menhir_stack _menhir_s _v) : 'freshtv61)) : 'freshtv63)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv64 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv65)) : 'freshtv67)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv74 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 16:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PARENTHESIS ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHT_PARENTHESIS) to state 17\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv70 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
            ((Printf.fprintf Pervasives.stderr "State 17:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv68 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
            ((Printf.fprintf Pervasives.stderr "Reducing production fragment_values -> LEFT_PARENTHESIS fragment_values RIGHT_PARENTHESIS \n%!";
            let ((_menhir_stack, _menhir_s), _, (f : 'tv_fragment_values)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_fragment_values = 
# 30 "src/parser.mly"
( [`Delimiter "("] @ f @ [`Delimiter ")"] )
# 175 "src/parser.ml"
             in
            _menhir_goto_fragment_values _menhir_env _menhir_stack _menhir_s _v) : 'freshtv69)) : 'freshtv71)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv72 * _menhir_state) * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv73)) : 'freshtv75)
    | MenhirState20 | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv76 * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 20:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            Printf.fprintf Pervasives.stderr "Shifting (EOF) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | LEFT_ANGLE_BRACKET ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFT_ANGLE_BRACKET) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | LEFT_BRACE ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFT_BRACE) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | LEFT_BRACKET ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFT_BRACKET) to state 4\n%!";
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | LEFT_PARENTHESIS ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFT_PARENTHESIS) to state 2\n%!";
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 1\n%!";
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20) : 'freshtv77)

and _menhir_goto_main : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "src/parser.mly"
       (Fragment.value list)
# 220 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 13 "src/parser.mly"
       (Fragment.value list)
# 231 "src/parser.ml"
        )) = _v in
        ((Printf.fprintf Pervasives.stderr "State 19:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : (
# 13 "src/parser.mly"
       (Fragment.value list)
# 240 "src/parser.ml"
        )) : (
# 13 "src/parser.mly"
       (Fragment.value list)
# 244 "src/parser.ml"
        )) = _v in
        ((Printf.fprintf Pervasives.stderr "Accepting\n%!";
        Obj.magic _1) : 'freshtv36)) : 'freshtv38)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41 * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 13 "src/parser.mly"
       (Fragment.value list)
# 255 "src/parser.ml"
        )) = _v in
        ((Printf.fprintf Pervasives.stderr "State 21:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((f : (
# 13 "src/parser.mly"
       (Fragment.value list)
# 264 "src/parser.ml"
        )) : (
# 13 "src/parser.mly"
       (Fragment.value list)
# 268 "src/parser.ml"
        )) = _v in
        ((Printf.fprintf Pervasives.stderr "Reducing production main -> fragment_values main \n%!";
        let (_menhir_stack, _menhir_s, (fragments : 'tv_fragment_values)) = _menhir_stack in
        let _v : (
# 13 "src/parser.mly"
       (Fragment.value list)
# 275 "src/parser.ml"
        ) = 
# 19 "src/parser.mly"
                                       ( fragments @ f)
# 279 "src/parser.ml"
         in
        _menhir_goto_main _menhir_env _menhir_stack _menhir_s _v) : 'freshtv40)) : 'freshtv42)
    | _ ->
        let (() : unit) = () in
        ((Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
        assert false) : 'freshtv43)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_fragment_values) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv34)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "src/parser.mly"
       (string)
# 323 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 1:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv21) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((s : (
# 1 "src/parser.mly"
       (string)
# 334 "src/parser.ml"
    )) : (
# 1 "src/parser.mly"
       (string)
# 338 "src/parser.ml"
    )) = _v in
    ((Printf.fprintf Pervasives.stderr "Reducing production fragment_values -> STRING \n%!";
    let _v : 'tv_fragment_values = 
# 41 "src/parser.mly"
             ( [`String s] )
# 344 "src/parser.ml"
     in
    _menhir_goto_fragment_values _menhir_env _menhir_stack _menhir_s _v) : 'freshtv22)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 2:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_ANGLE_BRACKET ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_ANGLE_BRACKET) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LEFT_BRACE ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_BRACE) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LEFT_BRACKET ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_BRACKET) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LEFT_PARENTHESIS ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_PARENTHESIS) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | RIGHT_PARENTHESIS ->
        Printf.fprintf Pervasives.stderr "Shifting (RIGHT_PARENTHESIS) to state 3\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState2 in
        ((Printf.fprintf Pervasives.stderr "State 3:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((Printf.fprintf Pervasives.stderr "Reducing production fragment_values -> LEFT_PARENTHESIS RIGHT_PARENTHESIS \n%!";
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_fragment_values = 
# 39 "src/parser.mly"
( [`Delimiter "("; `Delimiter ")"] )
# 384 "src/parser.ml"
         in
        _menhir_goto_fragment_values _menhir_env _menhir_stack _menhir_s _v) : 'freshtv18)) : 'freshtv20)
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 4:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_ANGLE_BRACKET ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_ANGLE_BRACKET) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LEFT_BRACE ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_BRACE) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LEFT_BRACKET ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_BRACKET) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LEFT_PARENTHESIS ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_PARENTHESIS) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | RIGHT_BRACKET ->
        Printf.fprintf Pervasives.stderr "Shifting (RIGHT_BRACKET) to state 5\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState4 in
        ((Printf.fprintf Pervasives.stderr "State 5:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((Printf.fprintf Pervasives.stderr "Reducing production fragment_values -> LEFT_BRACKET RIGHT_BRACKET \n%!";
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_fragment_values = 
# 35 "src/parser.mly"
( [`Delimiter "["; `Delimiter "]"] )
# 432 "src/parser.ml"
         in
        _menhir_goto_fragment_values _menhir_env _menhir_stack _menhir_s _v) : 'freshtv14)) : 'freshtv16)
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 6:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_ANGLE_BRACKET ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_ANGLE_BRACKET) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LEFT_BRACE ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_BRACE) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LEFT_BRACKET ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_BRACKET) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LEFT_PARENTHESIS ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_PARENTHESIS) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | RIGHT_BRACE ->
        Printf.fprintf Pervasives.stderr "Shifting (RIGHT_BRACE) to state 7\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState6 in
        ((Printf.fprintf Pervasives.stderr "State 7:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((Printf.fprintf Pervasives.stderr "Reducing production fragment_values -> LEFT_BRACE RIGHT_BRACE \n%!";
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_fragment_values = 
# 33 "src/parser.mly"
( [`Delimiter "{"; `Delimiter "}"] )
# 480 "src/parser.ml"
         in
        _menhir_goto_fragment_values _menhir_env _menhir_stack _menhir_s _v) : 'freshtv10)) : 'freshtv12)
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 8:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT_ANGLE_BRACKET ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_ANGLE_BRACKET) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LEFT_BRACE ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_BRACE) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LEFT_BRACKET ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_BRACKET) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LEFT_PARENTHESIS ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_PARENTHESIS) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | RIGHT_ANGLE_BRACKET ->
        Printf.fprintf Pervasives.stderr "Shifting (RIGHT_ANGLE_BRACKET) to state 9\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState8 in
        ((Printf.fprintf Pervasives.stderr "State 9:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((Printf.fprintf Pervasives.stderr "Reducing production fragment_values -> LEFT_ANGLE_BRACKET RIGHT_ANGLE_BRACKET \n%!";
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_fragment_values = 
# 37 "src/parser.mly"
( [`Delimiter "<"; `Delimiter ">"] )
# 528 "src/parser.ml"
         in
        _menhir_goto_fragment_values _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)) : 'freshtv8)
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 18:\n%!";
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((Printf.fprintf Pervasives.stderr "Reducing production main -> EOF \n%!";
    let _1 = () in
    let _v : (
# 13 "src/parser.mly"
       (Fragment.value list)
# 551 "src/parser.ml"
    ) = 
# 18 "src/parser.mly"
      ( [] )
# 555 "src/parser.ml"
     in
    _menhir_goto_main _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    Printf.fprintf Pervasives.stderr "Lookahead token is now %s (%d-%d)\n%!" (let (_tok : token) = _tok in
    ((match _tok with
    | EOF ->
        "EOF"
    | HOLE _ ->
        "HOLE"
    | LEFT_ANGLE_BRACKET ->
        "LEFT_ANGLE_BRACKET"
    | LEFT_BRACE ->
        "LEFT_BRACE"
    | LEFT_BRACKET ->
        "LEFT_BRACKET"
    | LEFT_PARENTHESIS ->
        "LEFT_PARENTHESIS"
    | RIGHT_ANGLE_BRACKET ->
        "RIGHT_ANGLE_BRACKET"
    | RIGHT_BRACE ->
        "RIGHT_BRACE"
    | RIGHT_BRACKET ->
        "RIGHT_BRACKET"
    | RIGHT_PARENTHESIS ->
        "RIGHT_PARENTHESIS"
    | STRING _ ->
        "STRING") : string)) lexbuf.Lexing.lex_start_p.Lexing.pos_cnum lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum;
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 13 "src/parser.mly"
       (Fragment.value list)
# 598 "src/parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((Printf.fprintf Pervasives.stderr "State 0:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        Printf.fprintf Pervasives.stderr "Shifting (EOF) to state 18\n%!";
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LEFT_ANGLE_BRACKET ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_ANGLE_BRACKET) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LEFT_BRACE ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_BRACE) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LEFT_BRACKET ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_BRACKET) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LEFT_PARENTHESIS ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFT_PARENTHESIS) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 219 "/Users/rvt/.opam/rooibos/lib/menhir/standard.mly"
  


# 646 "src/parser.ml"
