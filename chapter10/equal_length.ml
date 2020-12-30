(* 'a list -> 'a list -> bool *)
let rec equal_length lst1 lst2 =
  match (lst1, lst2) with
  | [], [] -> true
  | first :: rest, [] -> false
  | [], first :: rest -> false
  | f1 :: r1, f2 :: r2 -> equal_length r1 r2

let t1 = equal_length [] [] = true

let t2 = equal_length [ 1 ] [] = false

let t3 = equal_length [] [ 1 ] = false

let t4 = equal_length [ 1 ] [ 2 ] = true

let t5 = equal_length [ 1; 2; 3 ] [ 2; 3 ] = false

let t6 = equal_length [ 1; 2; 3 ] [ 2; 3; 42 ] = true
