open OUnit2
open Rooibos

let test_parser ctxt =
  let (!) s = Parser.main Lexer.read (Lexing.from_string s) in
  let matcher = !"(x(:[_]()))" in
  ()

let suite =
  "test" >::: [
    "test_parser" >:: test_parser
  ]

let () = run_test_tt_main suite
