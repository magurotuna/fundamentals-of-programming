(* int list -> int list *)
let even lst =
  let is_even x = x mod 2 = 0 in
  List.filter is_even lst

let test1 = even [ 1; 2; 3; 4; 5 ] = [ 2; 4 ]

let test2 = even [] = []

let test3 = even [ 1 ] = []

let test4 = even [ 0; 2; 4 ] = [ 0; 2; 4 ]

let rec filter pred lst =
  match lst with
  | [] -> []
  | first :: rest ->
      if pred first then first :: filter pred rest else filter pred rest

let rec length lst = match lst with [] -> 0 | first :: rest -> 1 + length rest

let count_A lst =
  let is_A x = x = "A" in
  length (filter is_A lst)

let t1 = count_A [] = 0

let t2 = count_A [ "A" ] = 1

let t3 = count_A [ "foo" ] = 0

let t4 = count_A [ "FOO"; "BAR"; "A"; "A"; "AA" ] = 2
