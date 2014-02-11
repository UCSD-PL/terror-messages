
open Lang

let wrapExp ?(pos=dummyPos) e         = { exp = e; pos = pos }
let rewrapExp p e                     = { e with pos = p }

let eInt ?(pos=dummyPos) i            = wrapExp ~pos (EConst (Int i))
let eBool ?(pos=dummyPos) b           = wrapExp ~pos (EConst (Bool b))
let eNil ?(pos=dummyPos) ()           = wrapExp ~pos (EConst Nil)

let eVar ?(pos=dummyPos) x            = wrapExp ~pos (EVar x)
let eLet ?(pos=dummyPos) p e1 e2      = wrapExp ~pos (ELet (p, e1, e2))
let eLetRec ?(pos=dummyPos) p e1 e2   = wrapExp ~pos (ELetRec (p, e1, e2))
let eFun ?(pos=dummyPos) p e          = wrapExp ~pos (EFun (p, e))
let eApp ?(pos=dummyPos) e1 e2        = wrapExp ~pos (EApp (e1, e2))
let eIf ?(pos=dummyPos) e1 e2 e3      = wrapExp ~pos (EIf (e1, e2, e3))

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
    (List.fold_left (fun acc p -> eFun p acc) eBody ps)

let eList ?(pos=dummyPos) es =
  rewrapExp pos
    (List.fold_left (fun acc e -> eCons e acc) (eNil ()) (List.rev es))

let rec eSeq : exp list -> exp =
function
  | []    -> failwith "eSeq: called with zero expressions"
  | [e]   -> e
  | e::es -> wrapExp (ELet (PVar "_", e, eSeq es))

