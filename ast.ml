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

type expr =
  | EConst of int
  | EBool of bool
  | EString of string
  | EVar of ident
  | EBexpr of bexpr
  | EBlock of block
  | ELam of funbody
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

type file = stmt list [@@deriving show]
