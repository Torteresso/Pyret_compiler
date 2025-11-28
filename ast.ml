type ident = string [@@deriving show]

type expr = EConst of int | EBool of bool | EString of string | EVar of ident
[@@deriving show]

type file = expr list [@@deriving show]
