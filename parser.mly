%{

%}

%token EOF

%start file

%type <Ast.file> file

%%

file:
    EOF { [] }
;

