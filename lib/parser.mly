%{
  open Core_kernel
  open Term

  let to_list x =
    match x with
    | Some x -> [x]
    | None  -> []
%}

%token <string> CONST
%token <string> HOLE
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_ANGLE
%token RIGHT_ANGLE
%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token EOF

%start <Term.t> main

%%

main:
| EOF        { Compound ("block", []) }
| block EOF  { $1 }

block:
| term       { $1 }
| terms      { Compound ("block", $1) }

terms:
| term       { [$1] }
| term terms { $1 :: $2 }

term:
| LEFT_BRACKET block? RIGHT_BRACKET         { Compound ("square", to_list $2) }
| LEFT_BRACE block? RIGHT_BRACE             { Compound ("curly",  to_list $2) }
| LEFT_ANGLE block? RIGHT_ANGLE             { Compound ("angle", to_list $2) }
| LEFT_PARENTHESIS block? RIGHT_PARENTHESIS { Compound ("round", to_list $2) }
| literal                                   { $1 }

(* TODO: disallow two holes next to each other, (or merge into one?) *)
literal:
| CONST { Const $1 }
| HOLE  { Var ($1, 0) }
