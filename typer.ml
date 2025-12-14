open Ast

exception Typer_error of loc * typ * typ
exception Typer_errorS of loc * string

module V = struct
  type t = tvar

  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id

  let create =
    let r = ref 0 in
    fun () ->
      incr r;
      { id = !r; def = None }
end

module Imap = Map.Make (Int)

let copyTyp t =
  let idEnv = ref Imap.empty in
  let rec process = function
    | (Number | String | Boolean | Any | Nothing) as t -> t
    | List t -> List (process t)
    | Arrow (tl, t) -> Arrow (List.map process tl, process t)
    | Tvar { id = i; def = None } -> (
        try
          let v = Imap.find i !idEnv in
          Tvar v
        with Not_found ->
          let v = V.create () in
          idEnv := Imap.add i v !idEnv;
          Tvar v)
    | Tvar { id = i; def = Some t } -> (
        try
          let v = Imap.find i !idEnv in
          Tvar v
        with Not_found ->
          let v = V.create () in
          v.def <- Some (process t);
          idEnv := Imap.add i v !idEnv;
          Tvar v)
  in
  process t

let rec head = function Tvar { def = Some t } -> head t | t -> t
let unification_error t1 t2 loc = raise (Typer_error (loc, t1, t2))

let rec occur var t =
  match head t with
  | Tvar v -> V.equal v var
  | List t1 -> occur var t1
  | Arrow (tl, t1) -> List.exists (occur var) tl || occur var t1
  | Number | String | Boolean | Any | Nothing -> false

let rec typToString2 t =
  match head t with
  | Number -> "Number"
  | String -> "String"
  | Boolean -> "Boolean"
  | Any -> "Any"
  | Nothing -> "Nothing"
  | List t -> "List<" ^ typToString2 t ^ ">"
  | Arrow (tl, t) ->
      "("
      ^ String.concat ", " (List.map typToString2 tl)
      ^ " -> " ^ typToString2 t ^ ")"
  | Tvar v -> "'a (id=" ^ string_of_int v.id ^ ")"

let reservedType = [ "Number"; "String"; "Boolean"; "Any"; "Nothing"; "List" ]

let rec unify loc t1 t2 =
  match (head t1, head t2) with
  | Number, Number -> ()
  | String, String -> ()
  | Boolean, Boolean -> ()
  | Nothing, Nothing -> ()
  | Any, t -> ()
  | t, Any -> ()
  | List t1, List t2 -> unify loc t1 t2
  | Arrow (tl1, t1), Arrow (tl2, t2) ->
      if List.length tl1 <> List.length tl2 then
        unification_error (Arrow (tl1, t1)) (Arrow (tl2, t2)) loc
      else (
        List.iter2 (unify loc) tl1 tl2;
        unify loc t1 t2)
  | Tvar v1, Tvar v2 when V.equal v1 v2 -> ()
  | Tvar v, t ->
      if occur v t then unification_error t1 t2 loc else v.def <- Some t
  | t, Tvar v -> unify loc t2 t1
  | _ -> unification_error t1 t2 loc

(* t1 <= t2 *)
let rec checkSubtype loc t1 t2 =
  match (head t1, head t2) with
  | t, Any -> ()
  | List t1, List t2 -> checkSubtype loc t1 t2
  | Arrow (tl1, t1), Arrow (tl2, t2) ->
      if List.length tl1 <> List.length tl2 then
        unification_error (Arrow (tl1, t1)) (Arrow (tl2, t2)) loc
      else checkSubtype loc t1 t2;
      List.iter2 (checkSubtype loc) tl2 tl1
  | Tvar v1, Tvar v2 when V.equal v1 v2 -> ()
  | t1, t2 -> if t1 = t2 then () else unification_error t2 t1 loc

module Vset = Set.Make (V)

let fvars t =
  let rec processType acc t =
    match head t with
    | Number | String | Boolean | Any | Nothing -> acc
    | List t1 -> processType acc t1
    | Arrow (tl, t1) ->
        Vset.union
          (List.fold_left (fun l t -> processType l t) acc tl)
          (processType acc t1)
    | Tvar v -> Vset.add v acc
  in
  processType Vset.empty t

type schema = { vars : Vset.t; typ : typ; isMutable : bool }

module Smap = Map.Make (String)

type env = { bindings : schema Smap.t; fvars : Vset.t }

let rec tyToTyp loc polymorEnv = function
  | PType (i, Some tl) -> (
      match (i, tl) with
      | "List", [ t ] -> List (tyToTyp loc polymorEnv t)
      | "List", _ ->
          raise
            (Typer_errorS
               ( loc,
                 "The type List expect only one parameter t of the form List<t>"
               ))
      | _ ->
          raise
            (Typer_errorS
               (loc, "The type " ^ i ^ " do not take other types as parameters"))
      )
  | PType (i, None) -> (
      match i with
      | "Number" -> Number
      | "String" -> String
      | "Boolean" -> Boolean
      | "Nothing" -> Nothing
      | "Any" -> Any
      | _ as s -> (
          try
            let v = Smap.find s polymorEnv in
            Tvar v
          with Not_found ->
            raise (Typer_errorS (loc, "The type " ^ i ^ " do not exist."))))
  | RType (tl, t) ->
      Arrow (List.map (tyToTyp loc polymorEnv) tl, tyToTyp loc polymorEnv t)

let initialEnv =
  let t = Nothing in
  let b =
    Smap.singleton "nothing" { vars = fvars t; typ = t; isMutable = false }
  in
  let t = Arrow ([ Number; Number ], Number) in
  let b =
    Smap.add "num-modulo" { vars = fvars t; typ = t; isMutable = false } b
  in
  let t = List (Tvar (V.create ())) in
  let b = Smap.add "empty" { vars = fvars t; typ = t; isMutable = false } b in
  let t =
    let a = V.create () in
    Arrow ([ Tvar a; List (Tvar a) ], List (Tvar a))
  in
  let b = Smap.add "link" { vars = fvars t; typ = t; isMutable = false } b in
  let t =
    let a = V.create () in
    Arrow ([ Tvar a ], Tvar a)
  in
  let b = Smap.add "print" { vars = fvars t; typ = t; isMutable = false } b in
  let t = Arrow ([ String ], Tvar (V.create ())) in
  let b = Smap.add "raise" { vars = fvars t; typ = t; isMutable = false } b in
  let t =
    let a = V.create () in
    let b = V.create () in
    Arrow ([ Arrow ([ Tvar a ], Tvar b); List (Tvar a) ], Nothing)
  in
  let b = Smap.add "each" { vars = fvars t; typ = t; isMutable = false } b in
  let t =
    let a = V.create () in
    let b = V.create () in
    Arrow ([ Arrow ([ Tvar a; Tvar b ], Tvar a); Tvar a; List (Tvar b) ], Tvar a)
  in
  let b = Smap.add "fold" { vars = fvars t; typ = t; isMutable = false } b in
  { bindings = b; fvars = Vset.empty }

let emptyEnv = { bindings = Smap.empty; fvars = Vset.empty }

let add s t env loc isMutable =
  if Smap.mem s env.bindings then
    raise
      (Typer_errorS
         ( loc,
           "This variable name conflits with a previously defined variable or \
            built-in variable." ))
  else
    let v = fvars t in
    {
      bindings =
        Smap.add s { vars = Vset.empty; typ = t; isMutable } env.bindings;
      fvars = Vset.union v env.fvars;
    }

let add_gen s t env loc isMutable =
  if Smap.mem s env.bindings then
    raise
      (Typer_errorS
         ( loc,
           "This variable name conflits with a previously defined variable or \
            built-in variable." ))
  else
    let v = fvars t in
    let vEnv =
      Vset.fold (fun v s -> Vset.union (fvars (Tvar v)) s) env.fvars Vset.empty
    in
    let vSelected = Vset.diff v vEnv in
    {
      bindings =
        Smap.add s { vars = vSelected; typ = t; isMutable } env.bindings;
      fvars = v;
    }

module Vmap = Map.Make (V)

let find s env =
  let { vars = v; typ = t } = Smap.find s env.bindings in
  let v =
    Vset.fold (fun v s -> Vmap.add v (Tvar (V.create ())) s) v Vmap.empty
  in
  let rec processT t =
    match head t with
    | Number -> Number
    | String -> String
    | Boolean -> Boolean
    | Any -> Any
    | Nothing -> Nothing
    | List t1 -> List (processT t1)
    | Arrow (tl, t1) -> Arrow (List.map processT tl, processT t1)
    | Tvar v1 as t -> ( try Vmap.find v1 v with Not_found -> t)
  in
  processT t

let check p =
  let rec typeExpr env expr =
    match expr.edesc with
    | EConst _ -> { edesc = expr.edesc; eloc = expr.eloc; etyp = Some Number }
    | EBool _ -> { edesc = expr.edesc; eloc = expr.eloc; etyp = Some Boolean }
    | EString _ -> { edesc = expr.edesc; eloc = expr.eloc; etyp = Some String }
    | EVar i -> (
        try
          let { typ = t; isMutable } = Smap.find i env.bindings in
          let varT = if isMutable then t else find i env in
          print_endline ("VARIABLE " ^ i);
          print_endline (typToString2 varT);
          { edesc = expr.edesc; eloc = expr.eloc; etyp = Some varT }
        with Not_found ->
          raise (Typer_errorS (expr.eloc, "This variable is not defined.")))
    | EBexpr be ->
        let be, t = typeBexpr env be in
        { edesc = EBexpr be; eloc = expr.eloc; etyp = t }
    | EBlock b ->
        let b, t = typeBlock env b in
        { edesc = EBlock b; eloc = expr.eloc; etyp = t }
    | ELam (pl, rt, b) ->
        let lamEnv, paramTypes =
          List.fold_left
            (fun (env1, ptL) (i, ty) ->
              let t = tyToTyp expr.eloc Smap.empty ty in
              (add i t env1 expr.eloc false, t :: ptL))
            (env, []) pl
        in
        let returnT = tyToTyp expr.eloc Smap.empty rt in
        let b, bt = typeBlock lamEnv b in
        unify expr.eloc returnT (Option.get bt);
        let lamType = Arrow (paramTypes, returnT) in
        { edesc = ELam (pl, rt, b); eloc = expr.eloc; etyp = Some lamType }
    | ECall (i, cl) -> (
        try
          let { typ = t } = Smap.find i env.bindings in

          let currentFunctionT = ref (copyTyp t) in
          let rec typeCaller = function
            | [] -> []
            | bel :: l ->
                let typedBeL, beTypes =
                  List.fold_right
                    (fun be (beL, tL) ->
                      let be, t = typeBexpr env be in
                      print_endline (typToString2 (Option.get t));
                      (be :: beL, Option.get t :: tL))
                    bel ([], [])
                in
                let v = Tvar (V.create ()) in
                unify expr.eloc !currentFunctionT (Arrow (beTypes, v));
                checkSubtype expr.eloc !currentFunctionT (Arrow (beTypes, v));
                currentFunctionT := head v;
                typedBeL :: typeCaller l
          in
          let typedCl = typeCaller cl in
          {
            edesc = ECall (i, typedCl);
            eloc = expr.eloc;
            etyp = Some !currentFunctionT;
          }
        with Not_found ->
          raise (Typer_errorS (expr.eloc, "This function is not defined.")))
    | ECases (ty, be, branchL) ->
        let be, t = typeBexpr env be in
        let caseT = tyToTyp expr.eloc Smap.empty ty in
        unify expr.eloc caseT (Option.get t);
        let typedBranch, branchTypes =
          List.fold_left
            (fun (tBranch, bTypes) (i, il, b) ->
              try
                let { typ = branchT } = Smap.find i env.bindings in
                match (branchT, il) with
                | Arrow (tl, rt), Some il ->
                    unify expr.eloc rt caseT;
                    if List.length tl <> List.length il then
                      raise
                        (Typer_errorS
                           ( expr.eloc,
                             "Wrong number of arguments provided to " ^ i ))
                    else
                      let branchEnv =
                        List.fold_left2
                          (fun env1 t i -> add i t env1 expr.eloc false)
                          env tl il
                      in
                      let b, t = typeBlock branchEnv b in

                      ((i, Some il, b) :: tBranch, t :: bTypes)
                | bt, None ->
                    unify expr.eloc bt caseT;
                    let b, t = typeBlock env b in
                    ((i, None, b) :: tBranch, t :: bTypes)
                | _ -> raise (Typer_error (expr.eloc, caseT, branchT))
              with Not_found ->
                raise
                  (Typer_errorS
                     ( expr.eloc,
                       "No defined data type correspond to this branch name : "
                       ^ i )))
            ([], []) branchL
        in
        let rec checkBranchesT = function
          | [] -> None
          | [ t ] -> t
          | t1 :: t2 :: l ->
              unify expr.eloc (Option.get t1) (Option.get t2);
              checkBranchesT (t2 :: l)
        in
        {
          edesc = ECases (ty, be, typedBranch);
          eloc = expr.eloc;
          etyp = checkBranchesT branchTypes;
        }
    | EIf (be, b, beBL, bElse) ->
        let be, t = typeBexpr env be in
        unify expr.eloc Boolean (Option.get t);
        let b, bt = typeBlock env b in
        let typedBeBL, blocksT =
          List.fold_right
            (fun (be1, b1) (accBeBL, accBT) ->
              let be1, t1 = typeBexpr env be1 in
              unify expr.eloc Boolean (Option.get t1);
              let b1, bt1 = typeBlock env b1 in
              ((be1, b1) :: accBeBL, bt1 :: accBT))
            beBL ([], [])
        in
        let bElse, bElseT = typeBlock env bElse in
        let blocksT = List.append blocksT [ bElseT; bt ] in
        let rec checkBlocksT = function
          | [] -> None
          | [ t ] -> t
          | t1 :: t2 :: l ->
              unify expr.eloc (Option.get t1) (Option.get t2);
              checkBlocksT (t2 :: l)
        in
        {
          edesc = EIf (be, b, typedBeBL, bElse);
          eloc = expr.eloc;
          etyp = checkBlocksT blocksT;
        }
  and typeBexpr env be =
    let e, opE = be in
    let typedE = typeExpr env e in
    let previousTypedE = ref typedE in
    let previousType = ref (Option.get typedE.etyp) in
    let beType = ref !previousType in
    let typedOpE =
      List.fold_right
        (fun (op, e) l ->
          let currentTypedE = typeExpr env e in
          let currentType = Option.get currentTypedE.etyp in

          (match head !previousType with
          | Number -> (
              let checkOtherType =
                unify currentTypedE.eloc Number currentType
              in
              match op with
              | Eq | Dif -> beType := Boolean
              | Add | Mul | Div | Sub -> checkOtherType
              | Inf | Sup | InfEq | SupEq ->
                  checkOtherType;
                  beType := Boolean
              | And | Or ->
                  raise
                    (Typer_error (!previousTypedE.eloc, Boolean, !previousType))
              )
          | String -> (
              match op with
              | Eq | Dif -> beType := Boolean
              | Add -> unify currentTypedE.eloc String currentType
              | Mul | Div | Sub | InfEq | SupEq | Inf | Sup ->
                  raise
                    (Typer_error (!previousTypedE.eloc, Number, !previousType))
              | And | Or ->
                  raise
                    (Typer_error (!previousTypedE.eloc, Boolean, !previousType))
              )
          | Boolean -> (
              match op with
              | Eq | Dif -> beType := Boolean
              | And | Or -> unify currentTypedE.eloc Boolean currentType
              | Add | Mul | Div | Sub | InfEq | SupEq | Inf | Sup ->
                  raise
                    (Typer_error (!previousTypedE.eloc, Number, !previousType)))
          | Tvar v -> (
              match head currentType with
              | Tvar v2 ->
                  raise
                    (Typer_errorS
                       ( currentTypedE.eloc,
                         "Binop expression between expression of non built-in \
                          types in not allowed." ))
              | _ -> (
                  match op with
                  | Eq | Dif -> beType := Boolean
                  | And | Or ->
                      unify !previousTypedE.eloc currentType !previousType
                  | Add | Mul | Div | Sub | InfEq | SupEq | Inf | Sup ->
                      unify !previousTypedE.eloc currentType !previousType))
          | Any | Nothing | List _ | Arrow _ -> (
              match op with
              | Eq | Dif -> beType := Boolean
              | Add | Mul | Div | Sub | InfEq | SupEq | Inf | Sup ->
                  raise
                    (Typer_error (!previousTypedE.eloc, Number, !previousType))
              | And | Or ->
                  raise
                    (Typer_error (!previousTypedE.eloc, Boolean, !previousType))
              ));

          previousType := currentType;
          previousTypedE := currentTypedE;
          (op, currentTypedE) :: l)
        opE []
    in
    ((typedE, typedOpE), Some !beType)
  and typeBlock env b =
    let b = typeStmts env b in
    let { styp = t } = List.hd (List.rev b) in
    (b, t)
  and typeStmts env = function
    | [] -> []
    | s :: l -> (
        match s.sdesc with
        | SBexpr be ->
            let be, t = typeBexpr env be in
            { sdesc = SBexpr be; sloc = s.sloc; styp = t } :: typeStmts env l
        | SDecl (isVar, i, ty, be) ->
            let be, t = typeBexpr env be in
            let env = add_gen i (Option.get t) env s.sloc isVar in
            (match ty with
            | None -> ()
            | Some ty ->
                unify s.sloc (tyToTyp s.sloc Smap.empty ty) (Option.get t));
            { sdesc = SDecl (isVar, i, ty, be); sloc = s.sloc; styp = t }
            :: typeStmts env l
        | SAffec (i, be) -> (
            try
              let { typ = varTyp; isMutable } = Smap.find i env.bindings in
              if isMutable then (
                let be, t = typeBexpr env be in
                unify s.sloc varTyp (Option.get t);
                { sdesc = SAffec (i, be); sloc = s.sloc; styp = Some varTyp }
                :: typeStmts env l)
              else
                raise
                  (Typer_errorS
                     ( s.sloc,
                       "Affection on non mutable variable is not allowed." ))
            with Not_found ->
              raise (Typer_errorS (s.sloc, "This variable is not defined.")))
        | SFun (i, il, (pl, rt, b)) ->
            if Smap.mem i env.bindings then
              raise (Typer_errorS (s.sloc, "This function is already defined"))
            else
              let polymorphEnv =
                match il with
                | None -> Smap.empty
                | Some il ->
                    List.fold_left
                      (fun acc i ->
                        if List.mem i reservedType then
                          raise
                            (Typer_errorS
                               ( s.sloc,
                                 "The type " ^ i
                                 ^ " is already a built-in data type, choose \
                                    another name." ))
                        else Smap.add i (V.create ()) acc)
                      Smap.empty il
              in
              let funEnv, paramTypes =
                List.fold_left
                  (fun (env1, ptL) (i, ty) ->
                    let t = tyToTyp s.sloc polymorphEnv ty in
                    (add i t env1 s.sloc false, t :: ptL))
                  (env, []) pl
              in
              let returnT = tyToTyp s.sloc polymorphEnv rt in
              let funType = Arrow (paramTypes, returnT) in
              let funEnv = add i funType funEnv s.sloc false in
              let b, bt = typeBlock funEnv b in
              unify s.sloc returnT (Option.get bt);
              let env = add i funType env s.sloc false in
              {
                sdesc = SFun (i, il, (pl, rt, b));
                sloc = s.sloc;
                styp = Some funType;
              }
              :: typeStmts env l)
  in
  typeStmts initialEnv p
