%token <string> STRING
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_BRACE
%token RIGHTBRACE
%token EOF

%start <string list> main

%%

main:
| v = EOF { [] }
;
