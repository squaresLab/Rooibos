open OUnit2
open Lexing

open Rooibos

let pp_position formatter lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.fprintf formatter "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let (!) s =
  let lexbuf = Lexing.from_string s in
  try Parser.main Lexer.read lexbuf |> ignore with
  | Parser.Error ->
    failwith @@ Format.asprintf "%a: syntax error in %s\n" pp_position lexbuf s

let assert_fails_with_message message f =
  assert_raises (Failure message) f

let test_parser _ =
  !"";
  !"x";
  !"x()x";
  !"()x()";
  !"{a{[x[z]y]({})}b}d";
  !"(x(:[_]()))";

  assert_fails_with_message
    ":1:2: syntax error in {\n"
    (fun () -> !"{");

  assert_fails_with_message
    ":1:11: syntax error in (x(:[_]())\n"
    (fun () -> !"(x(:[_]())");

  assert_fails_with_message
    "FIXME TWO OR MORE HOLES SHOULD FAIL"
    (fun () -> !":[_]:[_]:[_]")

let suite =
  "test" >::: [
    "test_parser" >:: test_parser
  ]

let () = run_test_tt_main suite
