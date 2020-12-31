(* int list -> int list *)
let sum_list lst =
  (* int list -> int -> int list *)
  (* lst is the target list to calculate accumulated sum *)
  (* acc is accumulated sum *)
  let rec inner_sum_list lst acc =
    match lst with
    | [] -> []
    | first :: rest ->
        let s = first + acc in
        s :: inner_sum_list rest s
  in
  inner_sum_list lst 0

let t1 = sum_list [] = []

let t2 = sum_list [ 1 ] = [ 1 ]

let t3 = sum_list [ 1; 2 ] = [ 1; 3 ]

let t4 = sum_list [ 3; 2; 1; 4 ] = [ 3; 5; 6; 10 ]
