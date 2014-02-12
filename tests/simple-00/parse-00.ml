let x = 0 in

let y = 1 in

let foo = fun x -> x + 1 in

let _ = foo 1 in

let z = foo 1 in

let foo x y = x + y + z in

(* blah *)

x + y
