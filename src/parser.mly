%token <string> STRING
%token <string> HOLE
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_ANGLE_BRACKET
%token RIGHT_ANGLE_BRACKET
%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token EOF

%start <Fragment.value list> main

%%

main:
| EOF { [] }
| fragments = fragment_values f = main { fragments @ f}
;

fragment_values:
| LEFT_BRACE; f = fragment_value; RIGHT_BRACE
{ [`Delimiter "{"; f; `Delimiter "}"] }
| LEFT_BRACE; ; RIGHT_BRACE
{ [`Delimiter "{"; `Delimiter "}"] }
| LEFT_BRACKET; f = fragment_value; RIGHT_BRACKET
{ [`Delimiter "["; f ; `Delimiter "]"] }
| LEFT_ANGLE_BRACKET; f = fragment_value; RIGHT_ANGLE_BRACKET
{ [`Delimiter "<"; f; `Delimiter ">"] }
| LEFT_PARENTHESIS; f = fragment_value; RIGHT_PARENTHESIS
{ [`Delimiter "("; f; `Delimiter ")"] }
| s = STRING { [`String s] }
| h = HOLE { [`Hole h] }
;

fragment_value:
| s = STRING { `String s }
| h = HOLE { `Hole h }
;