
let pr = Printf.printf
let spr = Printf.sprintf
let (|>) x f = f x

type pos = Lexing.position * Lexing.position 

let dummyPos = (Lexing.dummy_pos, Lexing.dummy_pos)

type binary_operator = string

type var = string

type kvar = string

type typ =
  | TInt | TBool | TStr | TUnit | TNil
  | TFun of typ * typ
  | TConstraintVar of kvar
  | TTuple of typ list
  | TPlaceholder

type pattern =
  | PVar of pvar
  | PTuple of pattern list

and pvar =
  { pvar : var
  ; pvarUnsolved : typ
  ; pvarSolved : typ
  }

type base_value =
  | Int of int
  | Bool of bool
  | Str of string
  | Unit
  | Nil

type exp_ =   
  | EConst of base_value
  | EVar of var
  | EFun of pattern * exp
  | EApp of exp * exp
  | EIf of exp * exp * exp
  | ELet of pattern * exp * exp
  | ELetRec of pattern * exp * exp
  | ETuple of exp list

and exp =
  { exp : exp_
  ; pos : pos
  ; tUnsolved : typ
  ; tSolved : typ
  }

type straint = typ * typ

module Constraints = Set.Make (struct
  type t = straint
  let compare = compare
end)

type constraints = Constraints.t

type type_env = (var * typ) list

exception LangParseError of string
exception LangTcError of string

