{
open Lexing
open Parser

exception SyntaxError of string
}

rule read = parse
| "[" { LEFT_BRACKET }
| "]" { RIGHT_BRACKET }
| "{" { LEFT_BRACE }
| "}" { RIGHT_BRACE }
| "<" { LEFT_ANGLE_BRACKET }
| ">" { RIGHT_ANGLE_BRACKET }
| "(" { LEFT_PARENTHESIS }
| ")" { RIGHT_PARENTHESIS }
| [^ '[' ']' '{' '}' '<' '>' '(' ')']*
{
  let buf = Buffer.create 17 in
  Buffer.add_string buf (Lexing.lexeme lexbuf);
  STRING (Buffer.contents buf)
}
| eof { EOF }
