open Core_kernel
open Rooibos
open Lexing

let () =
  let matcher = "(x(:[1]()))" in
  let source = "(x(y()))" in
  let syntax_matcher = Parser.main Lexer.read (Lexing.from_string matcher) in
  let syntax_source = Parser.main Lexer.read (Lexing.from_string source) in
  Format.printf "parsed matcher: %s@." @@ Term.to_string syntax_matcher;
  Format.printf "parsed source:  %s@." @@ Term.to_string syntax_source;
  let environment =
    try Unify.unify_terms (Environment.create ()) syntax_matcher syntax_source with
    | Unify.NoUnify -> failwith @@ sprintf "Could not unify %s and %s" matcher source
  in
  Format.printf "%s@." @@ Environment.to_string environment
