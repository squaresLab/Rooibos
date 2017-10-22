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
    let buf = Buffer.create 17 in
    let _ = Buffer.add_string buf (Lexing.lexeme lexbuf) in
    let name = Buffer.contents buf in
    let name = String.chop_prefix_exn name ~prefix:":[" in
    let name = String.chop_suffix_exn name ~suffix:"]" in
      HOLE name
  }
| [^ '[' ']' '{' '}' '<' '>' '(' ')']*
  {
    let buf = Buffer.create 17 in
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      CODE (Buffer.contents buf)
  }
| eof { EOF }
