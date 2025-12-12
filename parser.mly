%{
    open Ast
    
    let exprWithLoc pos exprDesc =
        { edesc = exprDesc; eloc = posToLoc pos; etyp=None }

    let stmtWithLoc pos stmtDesc =
        { sdesc = stmtDesc; sloc = posToLoc pos; styp=None }

    let checkBinopExpr = function 
        | [] -> true
        | (b, e) :: l -> List.for_all (fun (b1, e) -> b1 = b) l 

    let rec checkEndOfBlock = function 
        | [] -> raise (Parsing_error "A block must contain at least one statement") 
        | s :: [] -> 
                    (match s.sdesc with 
                        | SBexpr _ | SAffec _ -> true 
                        | SDecl _ | SFun _ -> false)
        | _ :: l -> checkEndOfBlock l

     let isValidColonBlock = function 
        | [] -> raise (Parsing_error "A block must contain at least one statement") 
        | b -> (if (List.fold_left (fun n s -> (
                    (match s.sdesc with 
                        | SBexpr _ | SAffec _ -> (n + 1) 
                        | SDecl _ | SFun _ -> n)
                              )) 0 b) > 1 then false else true)


    let defaultElse pos = 
        let error_expr = exprWithLoc pos (EString "error") in
        let call_expr = exprWithLoc pos (ECall ("raise", [[(error_expr, [])]])) in
        let bexpr = (call_expr, []) in
        [stmtWithLoc pos (SBexpr bexpr)]

    let getCorrectElse pos = function 
        | None -> defaultElse pos
        | Some x -> x

    let checkIfExpr elif el = 
        match elif with
            | [] -> (match el with 
                        | None -> raise (Parsing_error "An if-expression has only one branch")
                        | Some _ -> ())
            | _ :: _ -> ()

    let checkStmtSpacing stmtList = 
        List.fold_left (fun l s -> 
            let lineNumber = s.sloc.pos_lnum in     
            if List.mem lineNumber l
            then raise (Parsing_error ("Multiple statements on line " ^ string_of_int lineNumber ^ " is not allowed"))
            else lineNumber :: l
                       ) [] stmtList; ()          
   



%}

%token EOF
%token <int> CONST  
%token <string> STRING  
%token <string> IDENT
%token TRUE FALSE IF ELSE ELSECOLON
%token FOR FROM CASES END
%token LAM FUN VAR
%token ADD SUB MUL DIV EQ INF SUP EQEQ INFEQ SUPEQ DIF AND OR
%token SPACELEFTPAR LEFTPAR RIGHTPAR PIPE
%token COLON BLOCK RIGHTTYSYMBOL LEFTTYSYMBOL COMMA ARROW

%nonassoc IDENT
%nonassoc LEFTPAR

%start file

%type <Ast.file> file

%%

file:
    e=stmt*
    EOF { checkStmtSpacing e; e }

block:
    s=stmt+                         { if checkEndOfBlock s then (checkStmtSpacing s; s)
                                      else raise (Parsing_error "A block must finish with an expression or affectation")}

stmt:
    | be=bexpr                      { stmtWithLoc $endpos (SBexpr be) }
    | i=IDENT COLON EQ be=bexpr     { stmtWithLoc $endpos (SAffec (i, be)) }
    | VAR i=IDENT t=varTy? EQ be=bexpr       
                                    { stmtWithLoc $endpos (SDecl (true, i, t, be)) }
    | i=IDENT t=varTy? EQ be=bexpr          
                                    { stmtWithLoc $endpos (SDecl (false, i, t, be)) }
    | FUN i=IDENT is=pIdent? fb=funbody
                                    { stmtWithLoc $endpos (SFun (i, is, fb)) }

pIdent:
     anyInf is=separated_nonempty_list(COMMA, IDENT) pClosingSymbol 
                                    { is }
funbody:    
    | LEFTPAR ps=separated_list(COMMA, param) RIGHTPAR rt=rTy b=ublock END
                                    { (ps, rt, b) }

param:
    i=IDENT COLON COLON t=ty        { (i, t) } 

varTy:
    COLON COLON t=ty                { t }

ty:
    | i=IDENT ts=pTy? 
                                    { PType (i, ts) }
    | anyLeftPar ts=separated_list(COMMA, ty) rt=rTy RIGHTPAR
                                    { RType (ts, rt) }
pTy: 
     anyInf ts=separated_nonempty_list(COMMA, ty) pClosingSymbol
                                    { ts }
pClosingSymbol:
    | RIGHTTYSYMBOL                 { () }
    | SUP                           { () }

rTy:
    ARROW t=ty                      { t }

ublock:
    | COLON b=block                 { if isValidColonBlock b then b
                                      else raise (Parsing_error 
                                      "A block introduced with ':' cannot contains more than one expression or affectation")}
    | BLOCK b=block                 { b }

ublockSymbols:
    | COLON                         { () }
    | BLOCK                         { () }

bexpr: 
    e=expr be=binopExpr*            { if checkBinopExpr be then (e, be)
                                     else raise (Parsing_error "Cannot chain different binop operators")}

binopExpr:
    op=binop e=expr                 { (op, e) }

expr:
    | i=IDENT cs=caller+            { exprWithLoc $endpos (ECall (i, cs)) }
    | c=CONST                       { exprWithLoc $endpos (EConst c) } 
    | s=STRING                      { exprWithLoc $endpos (EString s) }
    | i=IDENT                       { exprWithLoc $endpos (EVar i) }
    | TRUE                          { exprWithLoc $endpos (EBool true) }  
    | FALSE                         { exprWithLoc $endpos (EBool false) }  
    | anyLeftPar be=bexpr RIGHTPAR  { exprWithLoc $endpos (EBexpr be) }
    | BLOCK b=block END             { exprWithLoc $endpos (EBlock b) }
    | LAM fb=funbody                { exprWithLoc $endpos (ELam fb) } 
    | FOR i=IDENT anyLeftPar fs=separated_list(COMMA, from) 
          RIGHTPAR rt=rTy b=ublock END
                                    { let (p, e) = List.split fs in 
                                    exprWithLoc $endpos (ECall(i, [[exprWithLoc $endpos (ELam (p, rt, b)), []] @ e])) }
    | CASES anyLeftPar t=ty RIGHTPAR be=bexpr ublockSymbols bs=branch* END
                                    { exprWithLoc $endpos (ECases (t, be, bs)) }
    | IF be=bexpr b=ublock besB=elseIf* bElse=else_? END
                                    { checkIfExpr besB bElse;
                                      exprWithLoc $endpos (EIf (be, b, besB, getCorrectElse $endpos bElse)) }
        
elseIf:
    ELSE IF be=bexpr COLON b=block  { (be, b) }

else_:
    ELSECOLON b=block              { b }

caller:
    | LEFTPAR bes=separated_list(COMMA, bexpr) RIGHTPAR
                                    { bes }

branch:
    | PIPE i=IDENT is=identList? EQ RIGHTTYSYMBOL b=block 
                                    { (i, is, b) }

identList:
    | anyLeftPar is=separated_list(COMMA, IDENT) RIGHTPAR 
                                    { is }

from:
    | p=param FROM be=bexpr         { (p, be) }

anyLeftPar:
    | LEFTPAR                       { () }
    | SPACELEFTPAR                  { () }

anyInf:
    | INF                           { () }
    | LEFTTYSYMBOL                  { () }


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
