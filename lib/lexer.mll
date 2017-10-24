{
open Core_kernel
open Lexing
open Parser

exception SyntaxError of string
}

let hole = ":[" ['a'-'z' 'A'-'Z' '0'-'9' '_']+ "]"
let const = [^ '[' ']' '{' '}' '<' '>' '(' ')']*

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
  let buf = Buffer.create 17 in
  Buffer.add_string buf (Lexing.lexeme lexbuf);
  let name = Buffer.contents buf in
  let name = String.chop_prefix_exn name ~prefix:":[" in
  let name = String.chop_suffix_exn name ~suffix:"]" in
  HOLE name
}
| const
{
  let buf = Buffer.create 17 in
  Buffer.add_string buf (Lexing.lexeme lexbuf);
  CONST (Buffer.contents buf)
}
| eof { EOF }
