open Core_kernel
open OUnit2
open Lexing

open Rooibos


let pp_position formatter lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.fprintf formatter "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let (!) s =
  let lexbuf = Lexing.from_string s in
  try Parser.main Lexer.read lexbuf with
  | Parser.Error ->
    failwith @@ Format.asprintf "%a: syntax error in %s\n" pp_position lexbuf s

let assert_fails_with_message message f =
  assert_raises (Failure message) f

let test_parser _ =
  !"" |> ignore;
  !"x" |> ignore;
  !"x()x" |> ignore;
  !"()x()" |> ignore;
  !"{a{[x[z]y]({})}b}d" |> ignore;
  !"(x(:[_]()))" |> ignore;

  assert_fails_with_message
    ":1:2: syntax error in {\n"
    (fun () -> !"{");

  assert_fails_with_message
    ":1:11: syntax error in (x(:[_]())\n"
    (fun () -> !"(x(:[_]())");

  assert_equal
    ~printer:Term.to_string
    (Compound ("terms",[Const "x"; Var ("1",0)]))
    (!"x:[1]");

    assert_equal
    ~printer:Term.to_string
    (Compound ("terms",[Const "xy"; Var ("1",0)]))
    (!"xy:[1]");

  assert_fails_with_message
    "FIXME TWO OR MORE HOLES SHOULD FAIL"
    (fun () -> !":[_]:[_]:[_]")


let test_unify _ =
  let unify = Unify.unify_terms (Environment.create ()) in

  let env = unify !"(x(:[1]()))" !"(x(y()))" in
  assert_equal
    (Term.Const "y")
    (Environment.lookup env ("1",0));

  let env = unify !"(:[2](:[1]()))" !"(x(y()))" in
  assert_equal
    ([Term.Const "y"; Term.Const "x"])
    ([Environment.lookup env ("1",0); Environment.lookup env ("2",0)]);

  let env = unify !":[1]" !"(x(y()))" in
  assert_equal
    ~printer:Term.to_string
    (Compound ("round", [ Const "x"
                        ; Compound ("round", [ Const "y"
                                             ; Compound ("round", [])])]))
    (Environment.lookup env ("1",0))


let suite =
  "test" >::: [
    "test_parser" >:: test_parser
  ; "test_unify" >:: test_unify
  ]

let () = run_test_tt_main suite
