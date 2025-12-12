%{
    open Ast
    
    let posToLoc (pos : Lexing.position) = 
        { pos_fname = pos.pos_fname; 
          pos_lnum = pos.pos_lnum;
          pos_bol = pos.pos_bol;
          pos_cnum = pos.pos_cnum}

    let with_loc pos desc =
        { desc; loc = posToLoc pos; typ=None }

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

     let isValidColonBlock = function 
        | [] -> raise (Parsing_error "A block must contain at least one statement") 
        | b -> (if (List.fold_left (fun n s -> (
                    (match s with 
                        | SBexpr _ | SAffec _ -> (n + 1) 
                        | SDecl _ | SFun _ -> n)
                              )) 0 b) > 1 then false else true)


    let defaultElse pos = 
        let error_expr = with_loc pos (EString "error") in
        let call_expr = with_loc pos (ECall ("raise", [[(error_expr, [])]])) in
        let bexpr = (call_expr, []) in
        [SBexpr bexpr]

    let getCorrectElse pos = function 
        | None -> defaultElse pos
        | Some x -> x

    let checkIfExpr elif el = 
        match elif with
            | [] -> (match el with 
                        | None -> raise (Parsing_error "An if-expression has only one branch")
                        | Some _ -> ())
            | _ :: _ -> ()

   



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
    | i=IDENT cs=caller+            { with_loc $endpos (ECall (i, cs)) }
    | c=CONST                       { with_loc $endpos (EConst c) } 
    | s=STRING                      { with_loc $endpos (EString s) }
    | i=IDENT                       { with_loc $endpos (EVar i) }
    | TRUE                          { with_loc $endpos (EBool true) }  
    | FALSE                         { with_loc $endpos (EBool false) }  
    | anyLeftPar be=bexpr RIGHTPAR  { with_loc $endpos (EBexpr be) }
    | BLOCK b=block END             { with_loc $endpos (EBlock b) }
    | LAM fb=funbody                { with_loc $endpos (ELam fb) } 
    | FOR i=IDENT anyLeftPar fs=separated_list(COMMA, from) 
          RIGHTPAR rt=rTy b=ublock END
                                    { let (p, e) = List.split fs in 
                                    with_loc $endpos (ECall(i, [[with_loc $endpos (ELam (p, rt, b)), []] @ e])) }
    | CASES anyLeftPar t=ty RIGHTPAR be=bexpr ublockSymbols bs=branch* END
                                    { with_loc $endpos (ECases (t, be, bs)) }
    | IF be=bexpr b=ublock besB=elseIf* bElse=else_? END
                                    { checkIfExpr besB bElse;
                                      with_loc $endpos (EIf (be, b, besB, getCorrectElse $endpos bElse)) }
        
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
