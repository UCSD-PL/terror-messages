
open Lang

let wrapExp ?(pos=dummyPos) e =
  { exp = e; pos = pos; tUnsolved = TPlaceholder; tSolved = TPlaceholder }

let rewrapExp p e                     = { e with pos = p }

let eInt ?(pos=dummyPos) i            = wrapExp ~pos (EConst (Int i))
let eBool ?(pos=dummyPos) b           = wrapExp ~pos (EConst (Bool b))
let eStr ?(pos=dummyPos) s            = wrapExp ~pos (EConst (Str s))
let eUnit ?(pos=dummyPos) ()          = wrapExp ~pos (EConst Unit)
let eNil ?(pos=dummyPos) ()           = wrapExp ~pos (EConst Nil)

let eVar ?(pos=dummyPos) x            = wrapExp ~pos (EVar x)
let eLet ?(pos=dummyPos) p e1 e2      = wrapExp ~pos (ELet (p, e1, e2))
let eLetRec ?(pos=dummyPos) p e1 e2   = wrapExp ~pos (ELetRec (p, e1, e2))
let eFun ?(pos=dummyPos) p e          = wrapExp ~pos (EFun (p, e))
let eApp ?(pos=dummyPos) e1 e2        = wrapExp ~pos (EApp (e1, e2))
let eIf ?(pos=dummyPos) e1 e2 e3      = wrapExp ~pos (EIf (e1, e2, e3))
let eTuple ?(pos=dummyPos) es         = wrapExp ~pos (ETuple es)

let opPlus    = "(+)"
let opMinus   = "(-)"
let opMul     = "(*)"
let opDiv     = "(/)"
let opCons    = "(::)"
let opLt      = "(<)"
let opLe      = "(<=)"
let opEq      = "(=)"
let opEqEq    = "(==)"
let opNe      = "(<>)"
let opAnd     = "(&&)"
let opOr      = "(&&)"

let infixOps =
 [ opPlus ; opMinus ; opMul ; opDiv ; opCons ; opLt ; opLe ;
   opEq ; opEqEq ; opNe ; opAnd ; opOr ]

let eBinOp op ?(pos=dummyPos) e1 e2   = eApp (eApp (eVar op) e1) e2
let ePlus                             = eBinOp "(+)"
let eMinus                            = eBinOp "(-)"
let eMul                              = eBinOp "(*)"
let eDiv                              = eBinOp "(/)"
let eCons                             = eBinOp "(::)"
let eLt                               = eBinOp "(<)"
let eLe                               = eBinOp "(<=)"
let eEq                               = eBinOp "(=)"
let eEqEq                             = eBinOp "(==)"
let eNe                               = eBinOp "(<>)"
let eAnd                              = eBinOp "(&&)"
let eOr                               = eBinOp "(||)"

let eFunCurried ?(pos=dummyPos) ps eBody =
  if List.length ps = 0 then failwith "eFunCurried: zero patterns";
  rewrapExp pos
    (List.fold_left (fun acc p -> eFun p acc) eBody (List.rev ps))

let eList ?(pos=dummyPos) es =
  rewrapExp pos
    (List.fold_left (fun acc e -> eCons e acc) (eNil ()) (List.rev es))

let pVar x =
  PVar { pvar = x; pvarUnsolved = TPlaceholder; pvarSolved = TPlaceholder }

let rec eSeq : exp list -> exp =
function
  | []    -> failwith "eSeq: called with zero expressions"
  | [e]   -> e
  | e::es -> wrapExp (ELet (pVar "_", e, eSeq es))

let tFun : typ -> typ -> typ =
fun t1 t2 -> TFun (t1, t2)

let rec tFunCurried : typ list -> typ -> typ =
fun tArgs tRet ->
  match tArgs with
    | t::[]     -> TFun (t, tRet)
    | t::tArgs' -> TFun (t, tFunCurried tArgs' tRet)
    | []        -> tRet

let tTuple : typ list -> typ =
fun ts -> TTuple ts

let rec strTyp = function
  | TPlaceholder     -> "XXX"
  | TInt             -> "int"
  | TBool            -> "bool"
  | TStr             -> "string"
  | TUnit            -> "unit"
  | TNil             -> "XXX list"
  | TConstraintVar x -> x
  | TFun (t1, t2)    -> spr "(%s -> %s)" (strTyp t1) (strTyp t2)
  | TTuple ts        -> spr "(%s)" (String.concat " * " (List.map strTyp ts))

let rec strPattern = function
  | PVar {pvar = x} -> x
  | PTuple l -> spr "(%s)" (String.concat ", " (List.map strPattern l))

let rec mapExp : (exp -> exp)
              -> (typ -> typ)
              -> (pattern -> pattern) -> exp -> exp =
fun fE fT fP e ->
  let fE_ e_ = fE { e with exp = e_ } in
  match e.exp with
  | EConst _ | EVar _ ->
      fE_ e.exp
  | EFun (p, e) ->
      fE_ (EFun (mapPat fT fP p, mapExp fE fT fP e)) 
  | EApp (e1, e2) ->
      fE_ (EApp (mapExp fE fT fP e1, mapExp fE fT fP e2))
  | EIf (e1, e2, e3) ->
      fE_ (EIf (mapExp fE fT fP e1, mapExp fE fT fP e2, mapExp fE fT fP e3))
  | ELet (p, e1, e2) ->
      fE_ (ELet (mapPat fT fP p, mapExp fE fT fP e1, mapExp fE fT fP e2))
  | ELetRec (p, e1, e2) ->
      fE_ (ELetRec (mapPat fT fP p, mapExp fE fT fP e1, mapExp fE fT fP e2))
  | ETuple es ->
      fE_ (ETuple (List.map (mapExp fE fT fP) es))

and mapTyp : (typ -> typ) -> (pattern -> pattern) -> typ -> typ =
fun fT fP t -> match t with
  | TInt | TBool | TStr | TUnit | TNil -> fT t
  | TConstraintVar _ -> fT t
  | TPlaceholder -> fT t
  | TFun (t1, t2) -> fT (TFun (mapTyp fT fP t1, mapTyp fT fP t2))
  | TTuple ts -> fT (TTuple (List.map (mapTyp fT fP) ts))

and mapPat : (typ -> typ) -> (pattern -> pattern) -> pattern -> pattern =
fun fT fP -> function
  | PTuple l -> fP (PTuple (List.map (mapPat fT fP) l))
  | PVar {pvar = x; pvarUnsolved = t1; pvarSolved = t2} ->
      let t1 = mapTyp fT fP t1 in
      let t2 = mapTyp fT fP t2 in
      fP (PVar {pvar = x; pvarUnsolved = t1; pvarSolved = t2})

