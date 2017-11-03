%{
  open Core_kernel
  open Exceptions
  open Term

  let to_list x =
    match x with
    | Some x -> [x]
    | None  -> []

  let wrap term (start : Lexing.position) (stop : Lexing.position) =
    let start = Location.make start in
    let stop = Location.make stop in
      Format.printf
        "%s\t[%d:%d::%d:%d]\n"
        (Term.to_string term)
        (Location.line_no start) (Location.column start)
        (Location.line_no stop) (Location.column stop);
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
| EOF            { wrap (Const "") $startpos $endpos }
| terms EOF      { $1 }

terms:
| term           { $1 }
| term_list      { wrap (Compound ("block", $1)) $startpos $endpos }

term_list:
| term           { [$1] }
| term term_list { $1 :: $2 }

term:
| LEFT_BRACKET     terms? RIGHT_BRACKET     { wrap (Compound ("square", to_list $2)) $startpos $endpos($3) }
| LEFT_BRACE       terms? RIGHT_BRACE       { wrap (Compound ("curly",  to_list $2)) $startpos $endpos($3) }
| LEFT_ANGLE       terms? RIGHT_ANGLE       { wrap (Compound ("angle",  to_list $2)) $startpos $endpos($3) }
| LEFT_PARENTHESIS terms? RIGHT_PARENTHESIS { wrap (Compound ("round",  to_list $2)) $startpos $endpos($3) }
| literal                                   { $1 }

literal:
| SEPARATOR   { wrap (Const $1) $startpos $endpos }
| LINE_BREAK  { wrap Break $startpos $endpos }
| CONST       { wrap (Const $1) $startpos $endpos }
| HOLE        { wrap (Var ($1, 0)) $startpos $endpos }
| HOLE HOLE   { raise (ParseError ("Please, no consecutive holes allowed")) }
