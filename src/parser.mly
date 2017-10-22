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
| fragments = fragment_list f = main { List.flatten fragments }
;

fragment_list:
| f = fragment_values { [f] }
| f = fragment_values rest = fragment_list { f::rest }

fragment_values:
| LEFT_BRACE; f = fragment_list; RIGHT_BRACE
{ [`Delimiter "{"] @ List.flatten f @ [`Delimiter "}"] }
| LEFT_BRACKET; f = fragment_list; RIGHT_BRACKET
{ [`Delimiter "["] @ List.flatten f @ [`Delimiter "]"] }
| LEFT_ANGLE_BRACKET; f = fragment_list; RIGHT_ANGLE_BRACKET
{ [`Delimiter "<"] @ List.flatten f @ [`Delimiter ">"] }
| LEFT_PARENTHESIS; f = fragment_list; RIGHT_PARENTHESIS
{ [`Delimiter "("] @ List.flatten f @ [`Delimiter ")"] }

| LEFT_BRACE; RIGHT_BRACE
{ [`Delimiter "{"; `Delimiter "}"] }
| LEFT_BRACKET; RIGHT_BRACKET
{ [`Delimiter "["; `Delimiter "]"] }
| LEFT_ANGLE_BRACKET; RIGHT_ANGLE_BRACKET
{ [`Delimiter "<"; `Delimiter ">"] }
| LEFT_PARENTHESIS; RIGHT_PARENTHESIS
{ [`Delimiter "("; `Delimiter ")"] }

| f = atomic_fragment { [f] }
;

atomic_fragment:
| s = STRING { `String s }
| h = HOLE { `Hole h }
