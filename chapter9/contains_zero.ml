let rec contains_zero lst =
  match lst with
  | [] -> false
  | first :: rest -> if first = 0 then true else contains_zero rest

(* tests *)
let t1 = contains_zero [] = false

let t2 = contains_zero [ 0 ] = true

let t3 = contains_zero [ 1 ] = false

let t4 = contains_zero [ 0; 2 ] = true

let t5 = contains_zero [ 1; 2 ] = false

let t6 = contains_zero [ 1; 2; 3; 4; 0; 5; 6 ] = true
