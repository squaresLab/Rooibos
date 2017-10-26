{
open Core_kernel
open Lexing
open Parser

exception SyntaxError of string
}

let hole = ":[" ['a'-'z' 'A'-'Z' '0'-'9' '_']+ "]"

rule read = parse
| "[" { LEFT_BRACKET }
| "]" { RIGHT_BRACKET }
| "{" { LEFT_BRACE }
| "}" { RIGHT_BRACE }
| "<" { LEFT_ANGLE }
| ">" { RIGHT_ANGLE }
| "(" { LEFT_PARENTHESIS }
| ")" { RIGHT_PARENTHESIS }
| hole
{
  let name = Lexing.lexeme lexbuf in
  let name = String.chop_prefix_exn name ~prefix:":[" in
  let name = String.chop_suffix_exn name ~suffix:"]" in
  HOLE name
}
| _ as c
{
  let buf = Buffer.create 17 in
  Buffer.add_char buf c;
  read_const buf lexbuf
}
| eof { EOF }

(* read until we hit whitespace, a new line, or some kind of delimiter *)
and read_const buf = parse
| ":[" | '[' | ']' | '{' | '}' | '<' | '>' | '(' | ')'
{
  let k = String.length (Lexing.lexeme lexbuf) in
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - k;
  CONST (Buffer.contents buf)
}
| _ as c  { Buffer.add_char buf c; read_const buf lexbuf }
| eof     { CONST (Buffer.contents buf) }
