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
  !"if (x < y) {}" |> ignore;

  assert_fails_with_message
    ":1:2: syntax error in {\n"
    (fun () -> !"{");

  assert_fails_with_message
    ":1:11: syntax error in (x(:[_]())\n"
    (fun () -> !"(x(:[_]())");

  assert_equal
    ~printer:Term.to_string
    (Compound ("block",[Const "x"; Var ("1",0)]))
    (!"x:[1]");

  assert_equal
    ~printer:Term.to_string
    (Compound ("block",[Const "xy"; Var ("1",0)]))
    (!"xy:[1]");

  assert_raises
    (Exceptions.ParseError  "Please, no consecutive holes allowed")
    (fun () -> !":[_]:[_]");

  assert_raises
    (Exceptions.ParseError  "Please, no consecutive holes allowed")
    (fun () -> !":[_]:[_]:[_]")



let make_env bindings =
  List.fold bindings
    ~init:(Environment.create ())
    ~f:(fun env (v, term) -> Environment.add env (v,0) term)

let env_of_result template source =
  Option.value_exn (Match.find template source) |> fst

let printer = Environment.to_string

let test_match _ =
  assert_equal
    ~printer
    (make_env [(("1"), !"foo")])
    (env_of_result !"x = :[1];" !"x = foo; x = bar;");

  assert_equal
    ~printer
    (make_env [("1", !"a"); ("2", !"b")])
    (env_of_result !"x = :[1] + :[2];" !"x = a + b; x = c + d;");

  assert_equal
    (make_env [("1", !"y")])
    (env_of_result !"(x(:[1]()))" !"(x(y()))");

  assert_equal
    (make_env [("1", !"y()")])
    (env_of_result !"(:[1])" !"(y())");

  assert_equal
    (make_env [("1", !"x = y")])
    (env_of_result !"{ :[1]; }" !"{ x = y; }");

  assert_equal
    (make_env [("1", !"x"); ("2", !"y()")])
    (env_of_result !"(:[1](:[2]))" !"(x(y()))");

  assert_equal
    (make_env [("1", !"x"); ("2", !"y")])
    (env_of_result !"(:[1](:[2]()))" !"(x(y()))");

  assert_equal
    (make_env [("1", !"x"); ("2", !"y")])
    (env_of_result !"(:[1](:[2]()))" !"(x(y()))");

  assert_equal
    (make_env [("1", !"(x(y()))")])
    (env_of_result !":[1]" !"(x(y()))");

  assert_equal
    (make_env [("1", !"x()x")])
    (env_of_result !":[1]" !"x()x");

  assert_equal
    (make_env [("1", !"z()")])
    (env_of_result !"x(y(:[1]))" !"x(y(z()))");

  assert_equal
    (make_env [("1", !"()")])
    (env_of_result !"x:[1]x" !"x()x");

  assert_equal
    (make_env [("1", !"()")])
    (env_of_result !"x:[1]x" !"x()x");

  assert_equal
    (make_env [("1", !"a,b,c"); ("2", !":")])
    (env_of_result !"x({(:[1])}:[2])x" !"x({(a,b,c)}:)x");

  assert_equal
    (make_env [("1", !"x"); ("2", !"y")])
    (env_of_result !":[1],:[2]" !"x,y");

  assert_equal
    (make_env [("1", !"x"); ("2", !"y")])
    (env_of_result !".:[1],:[2]." !".x,y.");

  assert_equal
    (make_env [("1", !"x"); ("2", !"y")])
    (env_of_result !"strcpy(:[1],:[2])" !"strcpy(x,y)");

  assert_equal
    (make_env [("1", !"3"); ("2", !"x = y")])
    (env_of_result !"if (x > :[1]) { :[2]; }" !"if (x > 3) { x = y; }");

  assert_equal
    (make_env [("1", !"f()"); ("2", !"x = y")])
    (env_of_result !"if (x > :[1]) { :[2]; }" !"if (x > f()) { x = y; }");

  assert_equal
    (make_env [("1", !"f()")])
    (env_of_result !"if (x <= :[1] <= 10)" !"if (x <= f() <= 10)");

  assert_equal
    (make_env [("1", !"f()()"); ("2", !"x = y")])
    (env_of_result !"if (x > :[1]) { :[2]; }" !"if (x > f()()) { x = y; }");

  assert_equal
    (make_env [("1", !"()()()()")])
    (env_of_result !"{:[1]}" !"{()()()()}");

  assert_equal
    (make_env [("1", !"()")])
    (env_of_result !"{()()():[1]}" !"{()()()()}");

  assert_equal
    (make_env [("1", !"()()")])
    (env_of_result !"{()()():[1]}" !"{()()()()()}");

  assert_equal
    (make_env [("1", !"()()()")])
    (env_of_result !"{():[1]}" !"{()()()()}");


  assert_equal
    (make_env [("1", !"[{x}[0]]"); ("2", !"{}")])
    (env_of_result !"if (x > f([][:[1]])()) :[2]" !"if (x > f([][[{x}[0]]])()) {}")

let not_handled_tests _ =
  (* this case does not match because we don't split up characters *)
  let _' _ =
    assert_equal
      ~printer:Environment.to_string
      (make_env [("1", !"a")])
      (env_of_result !":[1]a" !"aa")
  in

  (* this case does not match because we are matching:
     <C(H), C(a)> with
     <C(a), C(a)

     This hits the case where the suffix of C(H), C(a), matches the
     thing we are matching against (non-greedy), and stops matching.
     We are looking ahead one only.

     Question: would it be safe to
     continue sliding the source until we see the last C(a)?
     Answer: No. In general, we do not know when to stop without
     infinite look ahead. Consider this case:

     "a a :[1] a a a" to match with
     "a a a a  a a a"

     Now :[1] needs to match with "a a", but we would not know to collect up
     to two a's before the common suffixes match, becuase to kno that we need
     to look three characters ahead (not one). In the worst case we need to
     look ahead infinitely. In general, we should decide whether to just not
     bother with this case. *)

  let _' _ =
    assert_equal
      ~printer:Environment.to_string
      (make_env [("1", !"a")])
      (env_of_result !":[1] a" !"a a");

    assert_equal
      ~printer:Environment.to_string
      (make_env [("1", !"a a")])
      (env_of_result !":a [1] a" !"a a")
  in
  ()


  let suite =
    "test" >::: [
      "test_parser" >:: test_parser
    ; "test_match" >:: test_match
    ; "not_handled_tests" >:: not_handled_tests
    ]

let () = run_test_tt_main suite
