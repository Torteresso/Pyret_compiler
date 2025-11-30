type ident = string [@@deriving show]

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

type expr = EConst of int | EBool of bool | EString of string | EVar of ident
[@@deriving show]

type bexpr = expr * (binop * expr) list [@@deriving show]
type stmt = SBexpr of bexpr [@@deriving show]
type file = stmt list [@@deriving show]
