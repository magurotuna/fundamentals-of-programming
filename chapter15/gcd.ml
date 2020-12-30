let sort (n, m) = if m >= n then (n, m) else (m, n)

(* Calculates GCD *)
(* int -> int -> int *)
let rec gcd n m =
  let n, m = sort (n, m) in
  if n = 0 then m else gcd n (m mod n)

let test1 = gcd 2 1 = 1

let test2 = gcd 2 4 = 2

let test3 = gcd 0 0 = 0

let test4 = gcd 0 3 = 3

let test5 = gcd 6 6 = 6

let test6 = gcd 12 4 = 4

let test7 = gcd 4 12 = 4
