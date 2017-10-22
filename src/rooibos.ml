open Core_kernel
open Lexer
open Lexing

let () =
  let snippet = Lexing.from_string "x(:[arga], z(:[argb], 10))" in
  let result = Parser.main Lexer.read snippet in
  let _ = print_string "sup?\n" in
		Fragment.print result
