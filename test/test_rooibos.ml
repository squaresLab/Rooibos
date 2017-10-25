open Core_kernel
open OUnit2
open Lexing

open Rooibos

let pp_position formatter lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.fprintf formatter "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let (!!) s =
  let lexbuf = Lexing.from_string s in
  try Parser.main Lexer.read lexbuf with
  | Parser.Error ->
    failwith @@ Format.asprintf "%a: syntax error in %s\n" pp_position lexbuf s

let assert_fails_with_message message f =
  assert_raises (Failure message) f

let test_parser _ =
  !!"" |> ignore;
  !!"x" |> ignore;
  !!"x()x" |> ignore;
  !!"()x()" |> ignore;
  !!"{a{[x[z]y]({})}b}d" |> ignore;
  !!"(x(:[_]()))" |> ignore;

  assert_fails_with_message
    ":1:2: syntax error in {\n"
    (fun () -> !!"{");

  assert_fails_with_message
    ":1:11: syntax error in (x(:[_]())\n"
    (fun () -> !!"(x(:[_]())");

  assert_equal
    ~printer:Term.to_string
    (Compound ("block",[Const "x"; Var ("1",0)]))
    (!!"x:[1]");

  assert_equal
    ~printer:Term.to_string
    (Compound ("block",[Const "xy"; Var ("1",0)]))
    (!!"xy:[1]");

  assert_raises
    (Exceptions.ParseError  "Please, no consecutive holes allowed")
    (fun () -> !!":[_]:[_]");

  assert_raises
    (Exceptions.ParseError  "Please, no consecutive holes allowed")
    (fun () -> !!":[_]:[_]:[_]")


let test_unify _ =
  let unify = Unify.unify_terms (Environment.create ()) in

  let env = unify !!"(x(:[1]()))" !!"(x(y()))" in
  assert_equal
    (Term.Const "y")
    (Environment.lookup env ("1",0));

  let env = unify !!"(:[2](:[1]()))" !!"(x(y()))" in
  assert_equal
    ([Term.Const "y"; Term.Const "x"])
    ([Environment.lookup env ("1",0); Environment.lookup env ("2",0)]);

  let env = unify !!":[1]" !!"(x(y()))" in
  assert_equal
    ~printer:Term.to_string
    (!!"(x(y()))")
    (Environment.lookup env ("1",0));

  let env = unify !!":[1]" !!"x()x" in
  assert_equal
    ~printer:Term.to_string
    (!!"x()x")
    (Environment.lookup env ("1",0));

  let env = unify !!"x(y:[1])" !!"x(y(z()))" in
  assert_equal
    ~printer:Term.to_string
    (!!"(z())")
    (Environment.lookup env ("1",0));

  let env = unify !!"x(y(:[1]))" !!"x(y(z()))" in
  assert_equal
    ~printer:Term.to_string
    (!!"z()")
    (Environment.lookup env ("1",0));

  let env = unify !!"x:[1]x" !!"x()x" in
  assert_equal
    ~printer:Term.to_string
    (!!"()")
    (Environment.lookup env ("1",0));

  let env = unify !!"x({(:[1])}:[2])x" !!"x({(a,b,c)}:)x" in
  assert_equal
    ([!!"a,b,c"; !!":"])
    ([Environment.lookup env ("1",0); Environment.lookup env ("2",0)])


let test_smt_strings _ =
  let env = Environment.create () in
  let t1 : Term.t = !!"x = :[a] + 10;" in (* can't use '1' with z3 lol *)
  let t2 : Term.t = !!"x = y + 10;" in
  let _ =  Unify.unify_flat env t1 t2 in
  ()


(*let env = solve !!"x = z(10, :[1], y[0]);" !!"x = z(10, doot(0), y[0]);" in
  assert_equal
    (!!"doot(0)")
    (Environment.lookup env ("1",0));

  let f () = solve !!"foo.:[1].val = :[2]" !!"foo.val = 100" in
  assert_raises Unify.NoUnify f
*)

let suite =
  "test" >::: [
    "test_parser" >:: test_parser
  ; "test_unify" >:: test_unify
  ; "test_smt_strings" >:: test_smt_strings
  ]

let () = run_test_tt_main suite
