%{
  open Core_kernel
  open Term
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
| EOF         { Compound ("terms", []) }
| terms EOF  { Compound ("terms", $1) }

terms:
| term       { [$1] }
| term terms { $1 :: $2 }

term:
| LEFT_BRACKET terms? RIGHT_BRACKET         { Compound ("square", Option.value ~default:[] $2) }
| LEFT_BRACE terms? RIGHT_BRACE             { Compound ("curly", Option.value ~default:[] $2) }
| LEFT_ANGLE terms? RIGHT_ANGLE             { Compound ("angle", Option.value ~default:[] $2) }
| LEFT_PARENTHESIS terms? RIGHT_PARENTHESIS { Compound ("round", Option.value ~default:[] $2) }
| literal                                   { $1 }

(* TODO: disallow two holes next to each other, (or merge into one?) *)
literal:
| CONST { Const $1 }
| HOLE  { Var ($1, 0) }
