open Core_kernel
open Lexer
open Lexing

let () =
  let snippet = Lexing.from_string "{}()" in
  let result = Parser.main Lexer.read snippet in
  List.iter result ~f:(function
      | `Delimiter s -> Format.printf "Delim: %s@." s
      | `String s -> Format.printf "String: %s@." s
      | `Hole s -> Format.printf "Hole: %s@." s)
