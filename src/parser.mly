%token <string> CODE
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

%start <Fragment.t> main

%%

main:
| EOF                         { `Block([]) }
| block = fragment_block EOF  { block }
;

fragment_block:
| frags = list_of_fragments { `Block(frags) }
;

list_of_fragments:
| f = nested_fragment                          { [f] }
| f = nested_fragment rest = list_of_fragments { f::rest }
;

nested_fragment:
| LEFT_BRACKET; RIGHT_BRACKET                             { `Nested(`Square, `Block([])) }
| LEFT_BRACE; RIGHT_BRACE                                 { `Nested(`Curly, `Block([])) }
| LEFT_ANGLE; RIGHT_ANGLE                                 { `Nested(`Angle, `Block([])) }
| LEFT_PARENTHESIS; RIGHT_PARENTHESIS                     { `Nested(`Angle, `Block([])) }
| LEFT_BRACKET; b = fragment_block; RIGHT_BRACKET         { `Nested(`Square, b) }
| LEFT_BRACE; b = fragment_block; RIGHT_BRACE             { `Nested(`Curly, b) }
| LEFT_ANGLE; b = fragment_block; RIGHT_ANGLE             { `Nested(`Angle, b) }
| LEFT_PARENTHESIS; b = fragment_block; RIGHT_PARENTHESIS { `Nested(`Round, b) }
| f = atomic_fragment                                     { f }
;

atomic_fragment:
| id = HOLE   { `Hole(id) }
| s = CODE { `Code(s) }
;
