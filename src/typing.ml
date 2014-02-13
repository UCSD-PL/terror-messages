
open Lang
open LangUtils

let freshVar =
  let c = ref 0 in
  fun () ->
    incr c;
    spr "K%02d" !c

let rec freshVarsForPattern = function
  | PVar _   -> TConstraintVar (freshVar ())
  | PTuple l -> tTuple (List.map freshVarsForPattern l)

let rec addPattern : pattern -> typ -> type_env -> (pattern * type_env) =
fun p t g ->
  let rec foo = function
    | PVar ({pvar = x} as pv), t ->
        (PVar { pv with pvarUnsolved = t }, [(x, t)])
    | PTuple xs, TTuple ts when List.length xs = List.length ts ->
        List.fold_left (fun (acc1,acc2) xt ->
          match acc1 with
            | PVar _ -> failwith "impossible"
            | PTuple l ->
                let (p,g) = foo xt in
                (PTuple (l @ [p]), acc2 @ g)
        ) (PTuple [], []) (List.combine xs ts)
    | p, t ->
        failwith (spr "addPattern shape of pattern: %s %s"
          (strPattern p) (strTyp t))
  in
  let (p,g') = foo (p, t) in
  (p, g' @ g)

let rec occursIn : kvar -> typ -> bool =
fun k -> function
  | TInt | TBool | TStr | TUnit | TNil -> false
  | TConstraintVar k' -> k = k'
  | TFun (t1, t2) -> occursIn k t1 || occursIn k t2
  | TTuple ts -> List.exists (occursIn k) ts
  | TPlaceholder -> failwith "occursIn: TPlaceholder"

let wrapUnsolved : exp -> typ -> constraints -> (exp * typ * constraints) =
fun e t c ->
  let e = { e with tUnsolved = t } in
  (e, t, c)

let rec tcGen : type_env -> exp -> (exp * typ * constraints) =
fun g e -> match e.exp with
  | EConst Int _  -> wrapUnsolved e TInt Constraints.empty
  | EConst Bool _ -> wrapUnsolved e TBool Constraints.empty
  | EConst Str _  -> wrapUnsolved e TStr Constraints.empty
  | EConst Unit   -> wrapUnsolved e TUnit Constraints.empty
  | EConst Nil    -> wrapUnsolved e TNil Constraints.empty
  | EVar x ->
      if List.mem_assoc x g
      then wrapUnsolved e (List.assoc x g) Constraints.empty
      else raise (LangTcError (spr "var not found: %s" x))
  | EIf (e1, e2, e3) ->
      let (e1,t1,c1) = tcGen g e1 in
      let (e2,t2,c2) = tcGen g e2 in
      let (e3,t3,c3) = tcGen g e3 in
      let c =
        List.fold_left Constraints.union c1 [c2; c3]
          |> Constraints.add (t1, TBool)
          |> Constraints.add (t2, t3)
      in
      (eIf e1 e2 e3, t2, c)
  | EFun (p, e2) ->
      let t1 = freshVarsForPattern p in
      let (p,g) = addPattern p t1 g in
      let (e2,t2,c) = tcGen g e2 in
      wrapUnsolved (eFun p e2) (TFun (t1, t2)) c
  | EApp (e1, e2) ->
      let (e1,t1,c1) = tcGen g e1 in
      let (e2,t2,c2) = tcGen g e2 in
      let x = TConstraintVar (freshVar ()) in
      let c = Constraints.union c1 c2 in
      let c = Constraints.add (t1, TFun (t2, x)) c in
      let e1 = { e1 with tUnsolved = TFun (t2, x) } in
      let e2 = { e2 with tUnsolved = t2 } in
      wrapUnsolved (eApp e1 e2) x c
  | ELet (p, e1, e2) ->
      let (e1,t1,c1) = tcGen g e1 in
      let (p,g) = addPattern p t1 g in
      let (e2,t2,c2) = tcGen g e2 in
      (eLet p e1 e2, t2, Constraints.union c1 c2)
  | ELetRec (p, e1, e2) -> failwith "letrec"
  | ETuple es ->
      let (es,ts,cs) = Utils.unzip3 (List.map (tcGen g) es) in
      wrapUnsolved (eTuple es) (TTuple ts)
        (List.fold_left Constraints.union Constraints.empty cs)

let initTypeEnv : type_env = [
  ("(+)", tFunCurried [TInt; TInt] TInt);
]

type subst = (kvar * typ) list

let rec applySubstTyp : (kvar * typ) -> typ -> typ =
fun kt t ->
  let (ki,ti) = kt in
  match t with
  | TInt | TBool | TStr | TUnit | TNil -> t
  | TConstraintVar k when k = ki -> ti
  | TConstraintVar _ -> t
  | TFun (t1, t2) -> TFun (applySubstTyp kt t1, applySubstTyp kt t2)
  | TTuple ts -> TTuple (List.map (applySubstTyp kt) ts)
  | TPlaceholder -> t

let applySubstStraints : (kvar * typ) -> straint list -> straint list =
fun kt ->
  let foo = applySubstTyp kt in
  List.map (fun (t1,t2) -> (foo t1, foo t2))

let applySubst : subst -> typ -> typ =
fun subst t ->
  List.fold_left (fun acc kt -> applySubstTyp kt acc) t subst

type ('a, 'b) result =
  | Err of 'a
  | Ok of 'b

let rec unify : straint list -> (string, subst) result =
function
  | [] -> Ok []
  | (t1, t2) :: cs when t1 = t2 -> unify cs
  | (TConstraintVar k, t) :: cs
  | (t, TConstraintVar k) :: cs when not (occursIn k t) ->
      (match unify (applySubstStraints (k,t) cs) with
         | Ok l  -> Ok ((k, t) :: l)
         | err   -> err)
  | (TFun (t1, t2), TFun (t1', t2')) :: cs ->
      unify ((t1, t1') :: (t2, t2') :: cs)
  | (TTuple ts1, TTuple ts2) :: cs when List.length ts1 = List.length ts2 ->
      unify (List.combine ts1 ts2 @ cs)
  | (t1, t2) :: _ ->
      Err (spr "cannot unify: %s = %s" (strTyp t1) (strTyp t2))

let printConstraints c =
  pr "Constraints:\n\n";
  Constraints.iter (fun (t1,t2) -> pr "%s = %s\n" (strTyp t1) (strTyp t2)) c;
  pr "\n";
  ()

let printUnifier l =
  pr "Unifier:\n\n";
  List.iter (fun (ki,ti) -> pr "%s := %s\n" ki (strTyp ti)) l;
  pr "\n";
  ()

let fillInInferredTypes : subst -> exp -> exp =
fun unifier ->
  mapExp
    (fun e -> { e with tSolved = applySubst unifier e.tUnsolved })
    (fun t -> t)
    (function
       | PVar pvar ->
           let tx = applySubst unifier pvar.pvarUnsolved in
           PVar { pvar with pvarSolved = tx }
       | p -> p)

let tc : string -> exp -> unit =
fun fileName e ->
  let (e,t,c) = tcGen initTypeEnv e in
  printConstraints c;
  match unify (Constraints.elements c) with
    | Ok unifier -> begin
        printUnifier unifier;
        let t = applySubst unifier t in
        let e = fillInInferredTypes unifier e in
        pr "- : %s\n" (LangUtils.strTyp t);
        AcePrinter.print e (spr "%s.html" fileName);
      end
    | Err s -> begin
        pr "Unification Error:\n%s\n" s;
        AcePrinter.print e (spr "%s.html" fileName);
      end

