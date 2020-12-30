(* 'a -> 'a *)
let f1 x = x

(* 'a -> 'b -> 'a *)
let f2 x y = x

(* 'a -> 'b -> 'b *)
let f3 x y = y

(* 'a -> ('a -> 'b) -> 'b *)
let f4 x f = f x

(* ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c  *)
let f5 g h x = h (g x)

(* ('a -> 'b) -> ('c -> 'a) -> ('c -> 'b) *)
let compose f g =
  let h x = f (g x) in
  h

let time2 x = x * 2

let add3 x = x + 3

let composed_func = compose time2 add3

let test = composed_func 4 = 14

let twice f =
  let g x = f (f x) in
  g

let twice2 = twice twice
