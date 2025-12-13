open Ast

exception Typer_error of loc * typ * typ
exception Typer_errorS of loc * string

let rec tyToTyp loc = function
  | PType (i, Some tl) -> (
      match (i, tl) with
      | "List", [ t ] -> List (tyToTyp loc t)
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
      | "Any" -> Any
      | _ -> raise (Typer_errorS (loc, "The type " ^ i ^ " do not exist.")))
  | RType (tl, t) -> Arrow (List.map (tyToTyp loc) tl, tyToTyp loc t)

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

let rec head = function Tvar { def = Some t } -> head t | t -> t
let unification_error t1 t2 loc = raise (Typer_error (loc, t1, t2))

let rec occur var t =
  match head t with
  | Tvar v -> V.equal v var
  | List t1 -> occur var t1
  | Arrow (tl, t1) -> List.exists (occur var) tl || occur var t1
  | Number | String | Boolean | Any -> false

let rec unify loc t1 t2 =
  match (head t1, head t2) with
  | Number, Number -> ()
  | String, String -> ()
  | Boolean, Boolean -> ()
  | Any, t -> ()
  | t, Any -> ()
  | List t1, List t2 -> unify loc t1 t2
  | Arrow (tl1, t1), Arrow (tl2, t2) ->
      List.iter2 (unify loc) tl1 tl2;
      unify loc t1 t2
  | Tvar v1, Tvar v2 when V.equal v1 v2 -> ()
  | Tvar v, t ->
      if occur v t then unification_error t1 t2 loc else v.def <- Some t
  | t, Tvar v -> unify loc t2 t1
  | _ -> unification_error t1 t2 loc

module Vset = Set.Make (V)

let fvars t =
  let rec processType acc t =
    match head t with
    | Number | String | Boolean | Any -> acc
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

let empty = { bindings = Smap.empty; fvars = Vset.empty }

let add s t env loc isMutable =
  if Smap.mem s env.bindings then
    raise
      (Typer_errorS
         ( loc,
           "This variable name conflits with a previously defined variable or \
            build-in variable." ))
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
            build-in variable." ))
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
    | List t1 -> List (processT t1)
    | Arrow (tl, t1) -> Arrow (List.map processT tl, processT t1)
    | Tvar v1 as t -> ( try Vmap.find v1 v with Not_found -> t)
  in
  processT t

let check p =
  let rec typeExpr env expr =
    (match expr.edesc with
    | EConst _ -> expr.etyp <- Some Number
    | EBool _ -> expr.etyp <- Some Boolean
    | EString _ -> expr.etyp <- Some String);
    expr
  in
  let rec typeBexpr env be =
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
                if head currentType = Number then ()
                else
                  raise (Typer_error (currentTypedE.eloc, Number, currentType))
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
              | Add ->
                  if head currentType = String then ()
                  else
                    raise
                      (Typer_error (currentTypedE.eloc, String, currentType))
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
              | And | Or ->
                  if head currentType = Boolean then ()
                  else
                    raise
                      (Typer_error (currentTypedE.eloc, Boolean, currentType))
              | Add | Mul | Div | Sub | InfEq | SupEq | Inf | Sup ->
                  raise
                    (Typer_error (!previousTypedE.eloc, Number, !previousType)))
          | Tvar v ->
              raise
                (Typer_errorS
                   (!previousTypedE.eloc, "This variable is not defined."))
          | Any -> failwith "This expression should be specified at this point.");

          previousType := currentType;
          previousTypedE := currentTypedE;
          (op, currentTypedE) :: l)
        opE []
    in
    ((typedE, typedOpE), Some !beType)
  in
  let rec typeStmt env = function
    | [] -> []
    | s :: l -> (
        match s.sdesc with
        | SBexpr be ->
            let be, t = typeBexpr env be in
            { sdesc = SBexpr be; sloc = s.sloc; styp = t } :: typeStmt env l
        | SDecl (isVar, i, ty, be) ->
            let be, t = typeBexpr env be in
            let env = add_gen i (Option.get t) env s.sloc isVar in
            (match ty with
            | None -> ()
            | Some ty -> unify s.sloc (tyToTyp s.sloc ty) (Option.get t));
            { sdesc = SDecl (isVar, i, ty, be); sloc = s.sloc; styp = t }
            :: typeStmt env l
        | SAffec (i, be) -> (
            try
              let { typ = varTyp; isMutable } = Smap.find i env.bindings in
              if isMutable then (
                let be, t = typeBexpr env be in
                unify s.sloc varTyp (Option.get t);
                { sdesc = SAffec (i, be); sloc = s.sloc; styp = Some varTyp }
                :: typeStmt env l)
              else
                raise
                  (Typer_errorS
                     ( s.sloc,
                       "Affection on non mutable variable is not allowed." ))
            with Not_found ->
              raise (Typer_errorS (s.sloc, "This variable is not defined."))))
  in
  typeStmt empty p
