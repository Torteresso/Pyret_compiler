%{
    open Ast

    exception Parsing_error of string

    let checkBinopExpr = function 
        | [] -> true
        | (b, e) :: l -> List.for_all (fun (b1, e) -> b1 = b) l 

    let rec checkEndOfBlock = function 
        | [] -> raise (Parsing_error "A block must contain at least one statement") 
        | s :: [] -> 
                    (match s with 
                        | SBexpr _ | SAffec _ -> true 
                        | SDecl _ | SVarDecl _ -> false)
        | _ :: l -> checkEndOfBlock l
%}

%token EOF
%token <int> CONST  
%token <string> STRING  
%token <string> IDENT
%token TRUE FALSE IF ELSE
%token FOR FROM CASES END
%token LAM FUN VAR
%token ADD SUB MUL DIV EQ INF SUP EQEQ INFEQ SUPEQ DIF AND OR
%token LEFTPAR RIGHTPAR
%token COLON BLOCK

%start file

%type <Ast.file> file

%%

file:
    e=stmt*
    EOF { e }

block:
    s=stmt+                         { if checkEndOfBlock s then s
                                      else raise (Parsing_error "A block must finish with an expression or affectation")}

stmt:
    | be=bexpr                      { SBexpr be }
    | i=IDENT COLON EQ be=bexpr     { SAffec (i, be) }
    | VAR i=IDENT EQ be=bexpr       { SVarDecl (i, be) }
    | i=IDENT EQ be=bexpr           { SDecl (i, be) }

bexpr: 
    e=expr be=binopExpr*            { if checkBinopExpr be then (e, be)
                                     else raise (Parsing_error "Cannot chain different binop operators")}

binopExpr:
    op=binop e=expr                 { (op, e) }

expr:
    | c=CONST                       { EConst c } 
    | s=STRING                      { EString s }
    | i=IDENT                       { EVar i }
    | TRUE                          { EBool true }  
    | FALSE                         { EBool false }  
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
