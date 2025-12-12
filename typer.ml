open Ast

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
let rec canon t = match head t with (Tvar _ | Number) as t -> t

exception UnificationFailure of typ * typ

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))

let rec occur var t =
  match head t with Tvar v -> V.equal v var | Number -> false

let rec unify t1 t2 =
  match (head t1, head t2) with
  | Number, Number -> ()
  | Tvar v1, Tvar v2 when V.equal v1 v2 -> ()
  | Tvar v, t -> if occur v t then unification_error t1 t2 else v.def <- Some t
  | t, Tvar v -> unify t2 t1
  | _ -> unification_error t1 t2

module Vset = Set.Make (V)

let fvars t =
  let rec processType acc t =
    match head t with Number -> acc | Tvar v -> Vset.add v acc
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
    | Tvar v1 as t -> ( try Vmap.find v1 v with Not_found -> t)
  in
  processT t

let check p =
  let rec typeExpr env expr =
    (match expr.desc with EConst i -> expr.typ <- Some Number);
    expr
  in
  let rec typeBexpr env = function
    | e, [] -> SBexpr (typeExpr env e, [])
    | e1, (op, e2) :: l -> (
        match op with Add -> SBexpr (typeExpr env e1, []))
  in
  let rec typeStmt env f =
    List.fold_right
      (fun s l -> match s with SBexpr be -> typeBexpr env be :: l)
      f []
  in
  typeStmt empty p
