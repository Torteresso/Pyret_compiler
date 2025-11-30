%{
    open Ast
%}

%token EOF
%token <int> CONST  
%token <string> STRING  
%token <string> IDENT
%token AND OR TRUE FALSE IF ELSE
%token FOR FROM BLOCK CASES END
%token LAM FUN VAR
%token ADD SUB MUL DIV EQ INF SUP

%start file

%type <Ast.file> file

%%

file:
    e=stmt*
    EOF { e }

stmt:
    | be=bexpr                      { SBexpr be }

bexpr: 
    e=expr be=binop_expr*           { (e, be) }

binop_expr:
    op=binop e=expr                 { (op, e) }

expr:
    | c=CONST                       { EConst c } 
    | s=STRING                      { EString s }
    | i=IDENT                       { EVar i }

%inline binop:
    | ADD                           { Add } 
    | SUB                           { Sub } 
    | MUL                           { Mul } 
    | DIV                           { Div } 
    | AND                           { And } 
    | OR                            { Or } 
    | EQ EQ                         { Eq }  
    | INF                           { Inf } 
    | INF EQ                        { InfEq } 
    | SUP                           { Sup } 
    | SUP EQ                        { SupEq } 
    | INF SUP                       { Dif } 
