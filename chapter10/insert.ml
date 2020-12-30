(* int list -> int -> int list *)
let rec insert (sorted_lst : int list) (n : int) : int list =
  match sorted_lst with
  | [] -> [ n ]
  | first :: rest ->
      if n <= first then n :: sorted_lst else first :: insert rest n

let t0 = insert [] 5 = [ 5 ]

let t1 = insert [ 1; 3; 4; 7; 8 ] 5 = [ 1; 3; 4; 5; 7; 8 ]

let t2 = insert [ 1; 3 ] 0 = [ 0; 1; 3 ]

let t3 = insert [ 1 ] 0 = [ 0; 1 ]

let t4 = insert [ 0 ] 1 = [ 0; 1 ]

let t5 = insert [ 0 ] 0 = [ 0; 0 ]

(* insertion sort *)
let rec ins_sort (lst : int list) : int list =
  match lst with [] -> [] | first :: rest -> insert (ins_sort rest) first

let tt0 = ins_sort [] = []

let tt1 = ins_sort [ 1 ] = [ 1 ]

let tt2 = ins_sort [ 1; 0 ] = [ 0; 1 ]

let tt3 = ins_sort [ 0; 2; 1 ] = [ 0; 1; 2 ]

let tt4 = ins_sort [ 2; 1; 0; -1 ] = [ -1; 0; 1; 2 ]
