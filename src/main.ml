open Core_kernel
open Rooibos
open Lexing

let () =
  let matcher = "(x(:[1]()))" in
  let source = "(x(y()))" in
  let syntax_matcher = Parser.main Lexer.read (Lexing.from_string matcher) in
  let syntax_source = Parser.main Lexer.read (Lexing.from_string source) in
  Format.printf "parsed matcher: %s@." @@ Node.to_string syntax_matcher;
  Format.printf "parsed source:  %s@." @@ Node.to_string syntax_source;
