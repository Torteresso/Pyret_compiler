exception Parsing_error of string

type loc = { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
[@@deriving show]

type typ = Number | Tvar of tvar [@@deriving show]
and tvar = { id : int; mutable def : typ option } [@@deriving show]

type ident = string [@@deriving show]
type isVar = bool [@@deriving show]

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Eq
  | Inf
  | InfEq
  | Sup
  | SupEq
  | Dif
[@@deriving show]

(* PType : polymorphic type | RType : Return type *)
type ty = PType of ident * ty list option | RType of ty list * ty
[@@deriving show]

type param = ident * ty [@@deriving show]

type expr = { desc : desc; loc : loc; mutable typ : typ option }

and desc =
  | EConst of int
  | EBool of bool
  | EString of string
  | EVar of ident
  | EBexpr of bexpr
  | EBlock of block
  | ELam of funbody
  | ECall of ident * caller list
  | ECases of ty * bexpr * branch list
  | EIf of bexpr * block * (bexpr * block) list * block
[@@deriving show]

and bexpr = expr * (binop * expr) list [@@deriving show]

and stmt =
  | SBexpr of bexpr
  | SAffec of ident * bexpr
  | SDecl of isVar * ident * ty option * bexpr
  | SFun of ident * ident list option * funbody
[@@deriving show]

and block = stmt list [@@deriving show]
and funbody = param list * ty * block [@@deriving show]
and caller = bexpr list [@@deriving show]
and branch = ident * ident list option * block [@@deriving show]

type file = stmt list [@@deriving show]
