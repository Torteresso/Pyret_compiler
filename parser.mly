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
%token AND OR TRUE FALSE IF ELSE
%token FOR FROM BLOCK CASES END
%token LAM FUN VAR
%token ADD SUB MUL DIV EQ INF SUP
%token LEFTPAR RIGHTPAR

%start file

%type <Ast.file> file

%%

file:
    e=stmt*
    EOF { e }

stmt:
    | be=bexpr                      { SBexpr be }

bexpr: 
    e=expr be=binopExpr*           { if checkBinopExpr be then (e, be)
                                     else raise (Parsing_error "Cannot chain different binop operators")}

binopExpr:
    op=binop e=expr                 { (op, e) }

expr:
    | c=CONST                       { EConst c } 
    | s=STRING                      { EString s }
    | i=IDENT                       { EVar i }
    | LEFTPAR be=bexpr RIGHTPAR     { EBexpr be }

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
