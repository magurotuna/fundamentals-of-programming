(* fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
let rec fold_left f acc lst =
  match lst with [] -> acc | first :: rest -> fold_left f (f acc first) rest

(* int list -> int *)
let sum_all lst = fold_left ( + ) 0 lst

let sum_all_test1 = sum_all [] = 0

let sum_all_test2 = sum_all [ 1 ] = 1

let sum_all_test3 = sum_all [ 0; 1; 3 ] = 4

let sum_all_test4 = sum_all [ -1; 10; 30 ] = 39
