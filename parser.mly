%{
    open Ast

    let checkBinopExpr = function 
        | [] -> true
        | (b, e) :: l -> List.for_all (fun (b1, e) -> b1 = b) l 

    exception Parsing_error of string
%}

%token EOF
%token <int> CONST  
%token <string> STRING  
%token <string> IDENT
%token TRUE FALSE IF ELSE
%token FOR FROM BLOCK CASES END
%token LAM FUN VAR
%token ADD SUB MUL DIV EQ INF SUP EQEQ INFEQ SUPEQ DIF AND OR
%token LEFTPAR RIGHTPAR

%start file

%type <Ast.file> file

%%

file:
    e=stmt*
    EOF { e }

stmt:
    | be=bexpr                      { SBexpr be }

block:
    s=stmt+                         { s }   

bexpr: 
    e=expr be=binopExpr*            { if checkBinopExpr be then (e, be)
                                     else raise (Parsing_error "Cannot chain different binop operators")}

binopExpr:
    op=binop e=expr                 { (op, e) }

expr:
    | c=CONST                       { EConst c } 
    | s=STRING                      { EString s }
    | i=IDENT                       { EVar i }
    | LEFTPAR be=bexpr RIGHTPAR     { EBexpr be }
    | BLOCK b=block END             { EBlock b }

%inline binop:
    | ADD                           { Add } 
    | SUB                           { Sub } 
    | MUL                           { Mul } 
    | DIV                           { Div } 
    | AND                           { And } 
    | OR                            { Or } 
    | EQEQ                          { Eq }  
    | INF                           { Inf } 
    | INFEQ                         { InfEq } 
    | SUP                           { Sup } 
    | SUPEQ                         { SupEq } 
    | DIF                           { Dif } 
