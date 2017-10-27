%{
  open Core_kernel
  open Exceptions
  open Term

  let to_list x =
    match x with
    | Some x -> [x]
    | None  -> []

  let wrap term lexbuf =
    let open Lexing in
    let start = Location.make lexbuf.lex_start_p in
    let stop = Location.make lexbuf.lex_curr_p in
    let range = Location.Range.make start stop in
      Node.make term range
%}

%token <string> WHITESPACE
%token <string> SEPARATOR
%token <string> CONST
%token <string> HOLE
%token LEFT_BRACKET RIGHT_BRACKET
%token LEFT_BRACE RIGHT_BRACE
%token LEFT_ANGLE RIGHT_ANGLE
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS
%token LINE_BREAK
%token EOF

%start <Term.t Node.t> main

%%

main:
| EOF            { wrap (Const "") lexbuf }
| terms EOF      { $1 }

terms:
| term           { $1 }
| term_list      { wrap (Compound ("block", $1)) lexbuf }

term_list:
| term           { [$1] }
| term term_list { $1 :: $2 }

term:
| LEFT_BRACKET     terms? RIGHT_BRACKET     { wrap (Compound ("square", to_list $2)) lexbuf }
| LEFT_BRACE       terms? RIGHT_BRACE       { wrap (Compound ("curly",  to_list $2)) lexbuf }
| LEFT_ANGLE       terms? RIGHT_ANGLE       { wrap (Compound ("angle",  to_list $2)) lexbuf }
| LEFT_PARENTHESIS terms? RIGHT_PARENTHESIS { wrap (Compound ("round",  to_list $2)) lexbuf }
| literal                                   { $1 }

literal:
| SEPARATOR   { wrap (Const $1) lexbuf }
| LINE_BREAK  { wrap Break lexbuf }
| CONST       { wrap (Const $1) lexbuf }
| HOLE        { wrap (Var ($1, 0)) lexbuf }
| HOLE HOLE   { raise (ParseError ("Please, no consecutive holes allowed")) }
