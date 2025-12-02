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

type expr =
  | EConst of int
  | EBool of bool
  | EString of string
  | EVar of ident
  | EBexpr of bexpr
  | EBlock of block
[@@deriving show]

and bexpr = expr * (binop * expr) list [@@deriving show]

and stmt =
  | SBexpr of bexpr
  | SAffec of ident * bexpr
  | SVarDecl of ident * bexpr
  | SDecl of ident * bexpr
[@@deriving show]

and block = stmt list [@@deriving show]

type file = stmt list [@@deriving show]
