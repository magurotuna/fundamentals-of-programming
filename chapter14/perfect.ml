let rec enumerate n = if n = 0 then [] else n :: enumerate (n - 1)

(* 14.15 one_to_n: int -> int *)
let one_to_n n = List.fold_right ( + ) (enumerate n) 0

let one_to_n_test1 = one_to_n 1 = 1

let one_to_n_test2 = one_to_n 2 = 3

let one_to_n_test3 = one_to_n 3 = 6

(* 14.16 fac: int -> int *)
let fac n = List.fold_right ( * ) (enumerate n) 1

let fac_test1 = fac 0 = 1

let fac_test2 = fac 1 = 1

let fac_test3 = fac 2 = 2

let fac_test4 = fac 3 = 6
