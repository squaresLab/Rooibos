{
open Core
open Lexing
open Parser

exception SyntaxError of string

let dump lexbuf =
  let open Lexing in
  let to_s pos = Location.to_string (Location.make pos) in
  Printf.printf "Start, End: (%s, %s)\n" (to_s lexbuf.lex_start_p) (to_s lexbuf.lex_curr_p)

let lshift_start lexbuf k =
  lexbuf.lex_start_pos <- lexbuf.lex_curr_pos - k;
  lexbuf.lex_start_p <-
    { lexbuf.lex_start_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum - k }

let lshift_curr lexbuf k =
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - k;
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum - k }
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

(* read until we hit whitespace, a new line, or some kind of delimiter (including start of strings) *)
and read_const buf = parse
| ":[" | '[' | ']' | '{' | '}' | '(' | ')' | white | newline | separators | '\'' | '"' | eof
{
  let k = String.length (Lexing.lexeme lexbuf) in
  lshift_curr lexbuf k;
  lshift_start lexbuf (Buffer.length buf);
  CONST (Buffer.contents buf)
}
| eof
{
  lshift_start lexbuf ((Buffer.length buf) - 1);
  CONST (Buffer.contents buf)
}
| _ as c  {
  Buffer.add_char buf c;
  read_const buf lexbuf
}

and read_string_literal_double buf = parse
| '"' {

  let s = Format.sprintf "\"%s\"" (Buffer.contents buf) in
  lshift_start lexbuf (String.length s);
  CONST s
}
| [^ '"']+ {
  Buffer.add_string buf (Lexing.lexeme lexbuf);
  read_string_literal_double buf lexbuf
}
| eof { failwith "String is not terminated" }
| _ {
  let pos = lexbuf.lex_curr_p in
  let pos = Format.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
  failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf ^ ": " ^ pos)
}

(* FIXME 99% copypasto *)
and read_string_literal_single buf = parse
| '\'' {
  let s = Format.sprintf "'%s'" (Buffer.contents buf) in
  lshift_start lexbuf (String.length s);
  CONST s
}
| [^ '\'']+ {
  Buffer.add_string buf (Lexing.lexeme lexbuf);
  read_string_literal_single buf lexbuf
}
| eof { failwith "String is not terminated" }
| _  {
  let pos = lexbuf.lex_curr_p in
  let pos = Format.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
  failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf ^ ": " ^ pos)
}
