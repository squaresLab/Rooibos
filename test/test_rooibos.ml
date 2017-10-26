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

let unify = Unify.unify_terms (Environment.create ())

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




(** OMG I DONT KNOW  WHAT WE SHOULD DO HERE. Options:

    A term unified with :[1] (turn concrete vars into holes)
          i.e., "x = :[1]; x = [:1]"

    Two environments where :[1] = "foo" and :[1] = "bar"? that doesn't make sense...

*)
let what_should_this_give _ =
  let _ = unify !!"x = :[1];" !!"x = foo; x = bar;" in
  ()



let test_match_use_cases _ =
  let env = unify !!"strcpy(dst,src)" !!"strcpy(:[1],src)" in
  assert_equal
    (!!"dst")
    (Environment.lookup env ("1",0));

  let env = unify !!"strcpy(dst,src)" !!"strcpy(:[1],:[2])" in
  assert_equal
    ([!!"dst"; !!"src"])
    ([Environment.lookup env ("1",0); Environment.lookup env ("2",0)])


let format s =
  let s = s |> String.chop_prefix_exn ~prefix:"\n"
  in
  let leading_indentation =
    Option.value_exn (String.lfindi s ~f:(fun _ c -> c <> ' '))
  in
  s
  |> String.split ~on:'\n'
  |> List.map ~f:(Fn.flip String.drop_prefix leading_indentation)
  |> String.concat ~sep:"\n"


let test_strcpy_replace_use_case _ =
  let template = "strcpy(:[1],:[2])" in
  let env = unify !!"strcpy(dst,src)" !!template in
  assert_equal
    ("strcpy(src,dst)")
    (Printer.to_string (Environment.substitute env !!template))

let test_add_if_brackets_use_case _ =
  let matcher =
    {|
      if (:[1])
        ::[2]
    |}
    |> format
  in

  let source =
    {|
      if (foo() > bar())
        return -1;
    |}
    |> format
  in

  let rewrite =
    {|
      if (:[1]) {
        ::[2]
      }
    |}
    |> format
  in

  let result =
    {|
      if (foo() > bar()) {
        return -1;
      }
    |}
    |> format
  in

  let env = unify !!source !!matcher in
  assert_equal
    result
    (Printer.to_string (Environment.substitute env !!rewrite))

let test_printer _ =
  let term = !!"x()" in
  assert_equal
    ("x()")
    (Printer.to_string term);

  let term = !!":[1]" in
  assert_equal
    (":[1]")
    (Printer.to_string term);

  let env =
    let env = Environment.create () in
    Environment.add env ("1",0) (Term.Const ":)") in
  let term =
    !!":[1]"
    |> Environment.substitute env in
  assert_equal
    (":)")
    (Printer.to_string term);

  let env =
    let env = Environment.create () in
    Environment.add env ("1",0) (Term.Const "dst") in
  let term =
    !!"strcpy(:[1],src)"
    |> Environment.substitute env in
  assert_equal
    ("strcpy(dst,src)")
    (Printer.to_string term)

let suite =
  "test" >::: [
    "test_parser" >:: test_parser
  ; "test_unify" >:: test_unify
  (*; "test_match_use_cases" >:: test_match_use_cases
  ; "test_strcpy_replace_use_case" >:: test_strcpy_replace_use_case
    ; "test_add_if_brackets_use_case" >:: test_add_if_brackets_use_case*)
  ; "test_printer" >:: test_printer
  ]

let () = run_test_tt_main suite
