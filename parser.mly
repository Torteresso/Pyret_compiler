%{
    open Ast
%}

%token EOF
%token <int> CONST  
%token <string> STRING  

%start file

%type <Ast.file> file

%%

file:
    e=expr*
    EOF { e }
;
expr:
    | c=CONST                       { EConst c } 
    | s=STRING                      { EString s}

