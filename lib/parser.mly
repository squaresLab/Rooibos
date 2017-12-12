%{
  open Core
  open Exceptions
  open Term

  let to_list x =
    match x with
    | Some x -> [x]
    | None  -> []
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

%start <Term.t> main

%%

main:
| EOF
  { Const ("", (Location.Range.make $startpos $endpos)) }
| terms EOF
  { $1 }

terms:
| term
  { $1 }
| term_list
  { Compound ("block", $1, (Location.Range.make $startpos $endpos)) }

term_list:
| term           { [$1] }
| term term_list { $1 :: $2 }

term:
| LEFT_BRACKET     terms? RIGHT_BRACKET
  { Compound ("square", to_list $2, (Location.Range.make $startpos $endpos($3))) }
| LEFT_BRACE       terms? RIGHT_BRACE
  { Compound ("curly",  to_list $2, (Location.Range.make $startpos $endpos($3))) }
| LEFT_ANGLE       terms? RIGHT_ANGLE
  { Compound ("angle",  to_list $2, (Location.Range.make $startpos $endpos($3))) }
| LEFT_PARENTHESIS terms? RIGHT_PARENTHESIS
  { Compound ("round",  to_list $2, (Location.Range.make $startpos $endpos($3))) }
| literal
  { $1 }

literal:
| SEPARATOR
  { Const ($1, (Location.Range.make $startpos $endpos)) }
| LINE_BREAK
  { Break (Location.Range.make $startpos $endpos) }
| CONST
  { Const ($1, (Location.Range.make $startpos $endpos)) }
| HOLE
  { Var   ($1, 0, (Location.Range.make $startpos $endpos)) }
| WHITESPACE
  { White ($1, (Location.Range.make $startpos $endpos)) }
| HOLE HOLE
  { raise (ParseError ("Please, no consecutive holes allowed")) }
