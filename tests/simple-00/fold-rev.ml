let rec fold f acc xs =
  match xs with
    | []     -> [acc]
    | x::xs' -> fold f (f acc x) xs' in

let rev = fold (fun acc x -> x :: acc) [] in

let palin xs = rev xs = xs in

0
