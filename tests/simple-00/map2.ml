let map2 f xs ys =
  map (fun (a,b) -> f a b) (combine xs ys) in

let lst = map2 (fun (x,y) -> x + y) [1;2;3] [4;5;6] in

let ans = filter (fun x -> x == 0) list in

0
