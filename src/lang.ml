
let pr = Printf.printf
let spr = Printf.sprintf
let (|>) x f = f x

type pos = Lexing.position * Lexing.position 

let dummyPos = (Lexing.dummy_pos, Lexing.dummy_pos)

type binary_operator = string

type var = string

type pattern =
  | PVar of var
  | PTuple of pattern list

type kvar = string

type typ =
  | TInt | TBool | TStr | TNil
  | TFun of typ_pattern * typ
  | TConstraintVar of kvar

and typ_pattern =
  | PTyp of typ
  | PTupleTyp of typ_pattern list

type base_value =
  | Int of int
  | Bool of bool
  | Str of string
  | Nil

type exp_ =   
  | EConst of base_value
  | EVar of var
  | EFun of pattern * exp
  | EApp of exp * exp
  | EIf of exp * exp * exp
  | ELet of pattern * exp * exp
  | ELetRec of pattern * exp * exp

and exp =
  { exp : exp_
  ; pos : pos
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

