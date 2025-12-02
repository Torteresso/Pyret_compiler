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
                        | SDecl _ | SFun _ -> false)
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
%token COLON BLOCK RIGHTTYSYMBOL LEFTTYSYMBOL COMMA ARROW

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
    | VAR i=IDENT t=varTy? EQ be=bexpr       
                                    { SDecl (true, i, t, be) }
    | i=IDENT t=varTy? EQ be=bexpr          
                                    { SDecl (false, i, t, be) }
    | FUN i=IDENT is=pIdent? fb=funbody
                                    { SFun (i, is, fb) }

pIdent:
     LEFTTYSYMBOL is=separated_list(COMMA, IDENT) pClosingSymbol 
                                    { is }
funbody:    
    | LEFTPAR ps=separated_list(COMMA, param) RIGHTPAR rt=rTy ublock b=block END
                                    { (ps, rt, b) }

param:
    i=IDENT COLON COLON t=ty        { (i, t) } 

varTy:
    COLON COLON t=ty                { t }

ty:
    | i=IDENT ts=pTy? 
                                    { PType (i, ts) }
    | LEFTPAR ts=separated_list(COMMA, ty) rt=rTy RIGHTPAR
                                    { RType (ts, rt) }
pTy: 
     LEFTTYSYMBOL ts=separated_list(COMMA, ty) pClosingSymbol
                                    { ts }
pClosingSymbol:
    | RIGHTTYSYMBOL                 { () }
    | SUP                           { () }

rTy:
    ARROW t=ty                      { t }

ublock:
    | COLON                         { () }
    | BLOCK                         { () }

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
    | LAM fb=funbody                { ELam fb } 

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
