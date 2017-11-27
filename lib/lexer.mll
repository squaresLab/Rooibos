{
open Core
open Lexing
open Parser

exception SyntaxError of string
}

let newline = '\n' |'\r' | "\r\n"
let white = [' ' '\t']+
let hole = ":[" ['a'-'z' 'A'-'Z' '0'-'9' '_']+ "]"
let separators = ',' | ';' | ':' | '.' | '-' '>'

rule read = parse

| newline { LINE_BREAK }
| "[" { LEFT_BRACKET }
| "]" { RIGHT_BRACKET }
| "{" { LEFT_BRACE }
| "}" { RIGHT_BRACE }
(*| "<" { LEFT_ANGLE }*)
(*| ">" { RIGHT_ANGLE }*)
| "(" { LEFT_PARENTHESIS }
| ")" { RIGHT_PARENTHESIS }
| separators
{
  SEPARATOR (Lexing.lexeme lexbuf)
}
| hole
{
  let name = Lexing.lexeme lexbuf in
  let name = String.chop_prefix_exn name ~prefix:":[" in
  let name = String.chop_suffix_exn name ~suffix:"]" in
  HOLE name
}
| white { WHITESPACE (Lexing.lexeme lexbuf) }
| '"'   { read_string_literal_double (Buffer.create 17) lexbuf }
| '\''  { read_string_literal_single (Buffer.create 17) lexbuf }
| _ as c
{
  let buf = Buffer.create 17 in
  Buffer.add_char buf c;
  read_const buf lexbuf
}
| eof { EOF }

(* read until we hit whitespace, a new line, or some kind of delimiter *)
and read_const buf = parse
| ":[" | '[' | ']' | '{' | '}' | '(' | ')' | ' ' | '\t' | newline | separators
{
  let k = String.length (Lexing.lexeme lexbuf) in
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - k;
  CONST (Buffer.contents buf)
}
| _ as c  { Buffer.add_char buf c; read_const buf lexbuf }
| eof     { CONST (Buffer.contents buf) }

and read_string_literal_double buf = parse
| '"'      { CONST (Buffer.contents buf) }
| [^ '"' '\n']+
  { Buffer.add_string buf (Lexing.lexeme lexbuf);
    read_string_literal_double buf lexbuf
  }
| '\n'
| eof  { failwith "String is not terminated" }
| _    {
  let pos = lexbuf.lex_curr_p in
  let pos = Format.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
  failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf ^ ": " ^ pos) }

and read_string_literal_single buf = parse
| '\''      { CONST (Buffer.contents buf) }
| [^ '\'' '\n']+
  { Buffer.add_string buf (Lexing.lexeme lexbuf);
    read_string_literal_single buf lexbuf
  }
| '\n'
| eof  { failwith "String is not terminated" }
| _    {
  let pos = lexbuf.lex_curr_p in
  let pos = Format.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
  failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf ^ ": " ^ pos) }
