
open Lang
open LangUtils

let rec addPattern : pattern -> typ_pattern -> type_env -> type_env =
fun p tp g ->
  let rec foo = function
    | PVar x, PTyp t -> [(x, t)]
    | PTuple xs, PTupleTyp ts ->
        if List.length xs = List.length ts
        then List.concat (List.map foo (List.combine xs ts))
        else failwith "addPattern length"
    | _ ->
        failwith "addPattern shape of pattern"
  in
  (foo (p, tp)) @ g

let freshVar =
  let c = ref 0 in
  fun () ->
    incr c;
    spr "K%02d" !c

let rec freshVarsForPattern = function
  | PVar _   -> PTyp (TConstraintVar (freshVar ()))
  | PTuple l -> PTupleTyp (List.map freshVarsForPattern l)

let rec occursIn : kvar -> typ -> bool =
fun k -> function
  | TInt | TBool | TStr | TNil -> false
  | TConstraintVar k' -> k = k'
  | TFun (t1, t2) -> occursInPattern k t1 || occursIn k t2

and occursInPattern k = function
  | PTyp t      -> occursIn k t
  | PTupleTyp l -> List.exists (occursInPattern k) l

let rec tcGen : type_env -> exp -> (typ * constraints) =
fun g e -> match e.exp with
  | EConst Int _  -> (TInt, Constraints.empty)
  | EConst Bool _ -> (TBool, Constraints.empty)
  | EConst Str _  -> (TStr, Constraints.empty)
  | EConst Nil _  -> (TNil, Constraints.empty)
  | EVar x ->
      if List.mem_assoc x g
      then (List.assoc x g, Constraints.empty)
      else raise (LangTcError (spr "var not found: %s" x))
  | EIf (e1, e2, e3) ->
      let (t1,c1) = tcGen g e1 in
      let (t2,c2) = tcGen g e2 in
      let (t3,c3) = tcGen g e3 in
      let c =
        List.fold_left Constraints.union c1 [c2; c3]
          |> Constraints.add (t1, TBool)
          |> Constraints.add (t2, t3)
      in
      (t2, c)
  | EFun (p, e) ->
      let t1 = freshVarsForPattern p in
      let (t2,c) = tcGen (addPattern p t1 g) e in
      (TFun (t1, t2), c)
  | EApp (e1, e2) ->
      let (t1,c1) = tcGen g e1 in
      let (t2,c2) = tcGen g e2 in
      let x = TConstraintVar (freshVar ()) in
      let c = Constraints.union c1 c2 in
      let c = Constraints.add (t1, TFun (PTyp t2, x)) c in
      (x, c)
  | ELet (p, e1, e2) ->
      let (t1,c1) = tcGen g e1 in
      let (t2,c2) = tcGen (addPattern p (PTyp t1) g) e2 in
      (t2, Constraints.union c1 c2)
  | ELetRec (p, e1, e2) -> failwith "letrec"

let initTypeEnv : type_env = [
  ("(+)", tSimpleFun TInt (tSimpleFun TInt TInt));
]

type subst = (kvar * typ) list

let rec applySubstTyp : subst -> typ -> typ =
fun subst t -> match t with
  | TInt | TBool | TStr | TNil -> t
  | TConstraintVar k when List.mem_assoc k subst -> List.assoc k subst
  | TConstraintVar _ -> t
  | TFun (t1, t2) -> TFun (applySubstTypPattern subst t1, applySubstTyp subst t2)

and applySubstTypPattern : subst -> typ_pattern -> typ_pattern =
fun subst -> function
  | PTyp t   -> PTyp (applySubstTyp subst t)
  | PTupleTyp l -> PTupleTyp (List.map (applySubstTypPattern subst) l)

let applySubstStraints : subst -> straint list -> straint list =
fun subst ->
  let foo = applySubstTyp subst in
  List.map (fun (t1,t2) -> (foo t1, foo t2))

type ('a, 'b) result =
  | Err of 'a
  | Ok of 'b

let rec unify : straint list -> (string, subst) result =
function
  | [] -> Ok []
  | (t1, t2) :: cs when t1 = t2 -> unify cs
  | (TConstraintVar k, t) :: cs
  | (t, TConstraintVar k) :: cs when not (occursIn k t) ->
      (match unify (applySubstStraints [(k,t)] cs) with
         | Ok l  -> Ok ((k, t) :: l)
         | err   -> err)
  | (TFun (t1, t2), TFun (t1', t2')) :: cs ->
      unify (straintsOfPattern (t1, t1') @ [(t2, t2')] @ cs)
  | (t1, t2) :: _ ->
      Err (spr "cannot unify: %s = %s" (strTyp t1) (strTyp t2))

and straintsOfPattern : typ_pattern * typ_pattern -> straint list =
function
  | PTyp t1, PTyp t2 ->
      [(t1, t2)]
  | PTupleTyp ts1, PTupleTyp ts2 when List.length ts1 = List.length ts2 ->
      List.concat (List.map straintsOfPattern (List.combine ts1 ts2))
  | t1, t2 ->
      failwith (spr "straintsOfPattern: %s %s"
        (strTypPattern t1) (strTypPattern t2))

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

let tc : exp -> typ =
fun e ->
  let (t,c) = tcGen initTypeEnv e in
  printConstraints c;
  match unify (Constraints.elements c) with
    | Ok unifier -> begin
        printUnifier unifier;
        let t = applySubstTyp unifier t in
        t
      end
    | Err s -> begin
        pr "Unification Error:\n%s\n" s;
        t
      end

