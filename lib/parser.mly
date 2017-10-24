%{
  open Core_kernel
  open Term

  let to_block x =
    match x with
    | Some Compound ("block", [Var v]) -> [Var v]
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
| terms      { Compound ("block", $1) }

terms:
| term       { [$1] }
| term terms { $1 :: $2 }

term:
| LEFT_BRACKET block? RIGHT_BRACKET          { Compound ("square", to_block $2) }
| LEFT_PARENTHESIS block? RIGHT_PARENTHESIS  { Compound ("round", to_block $2) }
| LEFT_BRACE block? RIGHT_BRACE              { Compound ("curly", to_block $2) }
| literal                                    { $1 }

(* TODO: disallow two holes next to each other, (or merge into one?) *)
literal:
| CONST { Const $1 }
| HOLE  { Var ($1, 0) }
