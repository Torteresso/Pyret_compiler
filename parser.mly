%{
    open Ast
%}

%token EOF
%token <int> CONST  

%start file

%type <Ast.file> file

%%

file:
    e=expr*
    EOF { e }
;
expr:
    | c=CONST                       { EConst c } 

