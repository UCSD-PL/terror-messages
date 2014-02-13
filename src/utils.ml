
let unzip3 : ('a * 'b * 'c) list -> ('a list * 'b list * 'c list) =
fun l ->
  let rec foo (l1, l2, l3) = function
    | [] -> (List.rev l1, List.rev l2, List.rev l3)
    | (x,y,z)::l -> foo (x::l1, y::l2, z::l3) l
  in
  foo ([],[],[]) l

(* http://caml.inria.fr/pub/old_caml_site/Examples/oc/basics/explode.ml *)

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;

let implode l =
  let result = String.create (List.length l) in
  let rec imp i = function
  | [] -> result
  | c :: l -> result.[i] <- c; imp (i + 1) l in
  imp 0 l;;

