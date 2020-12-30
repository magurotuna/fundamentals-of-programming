(* int list -> int list *)
let rec even lst =
  match lst with
  | [] -> []
  | first :: rest -> if first mod 2 = 0 then first :: even rest else even rest

let t1 = even [] = []

let t2 = even [ 2; 1; 6; 4; 7 ] = [ 2; 6; 4 ]

let t3 = even [ 1 ] = []

let t4 = even [ 2 ] = [ 2 ]
