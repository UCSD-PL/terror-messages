let foo (f,g) = fun x -> (f x,g x);;

let a = (foo ( (fun x -> x*x), (fun x -> [x]) )) 4;;

let b = (foo ( (fun x -> -x), (fun x -> 0::x) )) (4,[4]);;
