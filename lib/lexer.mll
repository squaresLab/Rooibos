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

and read_const buf = parse
| ':' '['
{
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos-2;
  CONST (Buffer.contents buf)
}
| '[' | ']' | '{' | '}' | '<' | '>' | '(' | ')'
{
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos-1;
  CONST (Buffer.contents buf)
}
| _ as c  { Buffer.add_char buf c; read_const buf lexbuf }
| eof     { CONST (Buffer.contents buf) }
