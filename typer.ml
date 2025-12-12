open Ast

exception Typer_error of loc * typ * typ

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

let rec canon t =
  match head t with (Tvar _ | Number | String | Boolean) as t -> t

exception UnificationFailure of typ * typ

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))

let rec occur var t =
  match head t with
  | Tvar v -> V.equal v var
  | Number | String | Boolean -> false

let rec unify t1 t2 =
  match (head t1, head t2) with
  | Number, Number -> ()
  | String, String -> ()
  | Tvar v1, Tvar v2 when V.equal v1 v2 -> ()
  | Tvar v, t -> if occur v t then unification_error t1 t2 else v.def <- Some t
  | t, Tvar v -> unify t2 t1
  | _ -> unification_error t1 t2

module Vset = Set.Make (V)

let fvars t =
  let rec processType acc t =
    match head t with
    | Number | String | Boolean -> acc
    | Tvar v -> Vset.add v acc
  in
  processType Vset.empty t

type schema = { vars : Vset.t; typ : typ }

module Smap = Map.Make (String)

type env = { bindings : schema Smap.t; fvars : Vset.t }

let empty = { bindings = Smap.empty; fvars = Vset.empty }

let add s t env =
  let v = fvars t in
  {
    bindings = Smap.add s { vars = Vset.empty; typ = t } env.bindings;
    fvars = Vset.union v env.fvars;
  }

let add_gen s t env =
  let v = fvars t in
  let vEnv =
    Vset.fold (fun v s -> Vset.union (fvars (Tvar v)) s) env.fvars Vset.empty
  in
  let vSelected = Vset.diff v vEnv in
  {
    bindings = Smap.add s { vars = vSelected; typ = t } env.bindings;
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

          (match !previousType with
          | Number -> (
              let checkOtherType =
                if currentType = Number then ()
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
                  if currentType = String then ()
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
                  if currentType = Boolean then ()
                  else
                    raise
                      (Typer_error (currentTypedE.eloc, Boolean, currentType))
              | Add | Mul | Div | Sub | InfEq | SupEq | Inf | Sup ->
                  raise
                    (Typer_error (!previousTypedE.eloc, Number, !previousType))));

          previousType := currentType;
          previousTypedE := currentTypedE;
          (op, currentTypedE) :: l)
        opE []
    in
    ((typedE, typedOpE), Some !beType)
  in
  let rec typeStmt env f =
    List.fold_right
      (fun s l ->
        match s.sdesc with
        | SBexpr be ->
            let be, t = typeBexpr env be in
            { sdesc = SBexpr be; sloc = s.sloc; styp = t } :: l)
      f []
  in
  typeStmt empty p
