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
| _   { read_string (Buffer.create 17) lexbuf }
| eof { EOF }

(* parse until we hit a delimiter *)
and read_string buf =
parse
| '{' | '}' | '[' | ']' | '<' | '>' | '(' | ')'
{ STRING (Buffer.contents buf) }
| _
{ Buffer.add_string buf (Lexing.lexeme lexbuf);
  read_string buf lexbuf
}
| eof { STRING (Buffer.contents buf) }
