exception Parsing_error of string

type loc = { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
[@@deriving show]

let posToLoc (pos : Lexing.position) =
  {
    pos_fname = pos.pos_fname;
    pos_lnum = pos.pos_lnum;
    pos_bol = pos.pos_bol;
    pos_cnum = pos.pos_cnum;
  }

type typ =
  | Number
  | String
  | Boolean
  | Any
  | Nothing
  | List of typ
  | Arrow of typ list * typ
  | Tvar of tvar
[@@deriving show]

and tvar = { id : int; mutable def : typ option } [@@deriving show]

type ident = string [@@deriving show]

(* PType : polymorphic type | FType : Return type *)
type ty = PType of ident * ty list option | FType of ty list * ty
[@@deriving show]

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

type param = ident * ty [@@deriving show]

type expr = { edesc : exprDesc; eloc : loc; mutable etyp : typ option }
[@@deriving show]

and exprDesc =
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

and stmt = { sdesc : stmtDesc; sloc : loc; mutable styp : typ option }
[@@deriving show]

and stmtDesc =
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
type frameSize = int [@@deriving show]

type exprC = { edescC : exprDescC; elocC : loc; mutable etypC : typ option }
[@@deriving show]

and var =
  | Vglobal of ident
  | Vlocal of ident * int
  | Vclos of ident * int
  | Varg of ident * int

and exprDescC =
  | CConst of int
  | CBool of bool
  | CString of string
  | CVar of var
  | CEBexpr of bexprC
  | CBlock of blockC
  | CClos of ident * var list
  | CCall of ident * var * callerC list
  | CCases of ty * bexprC * branchC list
  | CIf of bexprC * blockC * (bexprC * blockC) list * blockC
[@@deriving show]

and paramC = var * ty [@@deriving show]
and bexprC = exprC * (binop * exprC) list [@@deriving show]
and blockC = stmtC list [@@deriving show]
and callerC = bexprC list [@@deriving show]
and branchC = ident * var list * blockC [@@deriving show]

and stmtC = { sdescC : stmtDescC; slocC : loc; mutable stypC : typ option }
[@@deriving show]

and stmtDescC =
  | CBexpr of bexprC * frameSize
  | CAffec of ident * bexprC * frameSize
  | CDecl of isVar * var * ty option * bexprC * frameSize
  | CFun of ident * ident list option * ty * exprC * int
  | CLetFun of ident * paramC list * blockC * frameSize
[@@deriving show]

type fileC = stmtC list [@@deriving show]
