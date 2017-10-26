%{
  open Core_kernel
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
| EOF            { Const "" }
| terms EOF      { $1 }

terms:
| term           { $1 }
| term_list      { Compound ("block", $1) }

term_list:
| term           { [$1] }
| term term_list { $1 :: $2 }

term:
| LEFT_BRACKET     terms? RIGHT_BRACKET     { Compound ("square", to_list $2) }
| LEFT_BRACE       terms? RIGHT_BRACE       { Compound ("curly",  to_list $2) }
| LEFT_ANGLE       terms? RIGHT_ANGLE       { Compound ("angle",  to_list $2) }
| LEFT_PARENTHESIS terms? RIGHT_PARENTHESIS { Compound ("round",  to_list $2) }
| literal                                   { $1 }

literal:
| SEPARATOR { Const $1 }
| LINE_BREAK { Break }
| CONST     { Const $1 }
| HOLE      { Var ($1, 0) }
| HOLE HOLE { raise (ParseError ("Please, no consecutive holes allowed")) }
