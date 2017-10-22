open Core_kernel
open Lexer
open Lexing
open Syntax
open Unify

let term_to_syntax = function
  | `Delimiter s -> Const s
  | `String s -> Const s
  | `Hole s -> Var (s,0)

let () =
  let matcher = "(x(%1()))" in
  let source = "(x(y()))" in
  let matcher_ast = Parser.main Lexer.read (Lexing.from_string matcher) in
  let matcher_ast : Fragment.value list =
    [`Delimiter "("; `String "x"; `Delimiter "("; `Hole "%1"; `Delimiter "("
    ;`Delimiter ")"; `Delimiter ")"; `Delimiter ")"]
  in
  let source_ast = Parser.main Lexer.read (Lexing.from_string source) in
  List.iter matcher_ast ~f:(function
      | `Delimiter s -> Format.printf "Delim: %s@." s
      | `String s -> Format.printf "String: %s@." s
      | `Hole s -> Format.printf "Hole: %s@." s);
  let syntax_matcher = List.map matcher_ast ~f:term_to_syntax in
  let syntax_source = List.map source_ast ~f:term_to_syntax in
  let env =
    try Unify.unify_terms [] (App ("f",syntax_matcher)) (App ("f",syntax_source)) with
    | NoUnify -> failwith @@ sprintf "Could not unify %s and %s" matcher source
  in
  Format.printf "%s" @@ string_of_env env
