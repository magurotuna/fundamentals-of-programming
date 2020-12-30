(* int -> int list *)
let rec gen_list n = if n <= 1 then [] else gen_list (n - 1) @ [ n ]

let gen_list_test1 = gen_list 1 = []

let gen_list_test2 = gen_list 2 = [ 2 ]

let gen_list_test1 = gen_list 4 = [ 2; 3; 4 ]

(* int list -> int list -> int list *)
let rec sieve lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | first :: rest ->
      let filter_div lst div = List.filter (fun x -> x mod div <> 0) lst in
      let filtered = filter_div rest first in
      let primes = first :: lst2 in
      sieve filtered primes

let rec sieve2 lst =
  match lst with
  | [] -> []
  | first :: rest ->
      first :: sieve2 (List.filter (fun x -> x mod first <> 0) rest)

let sieve_test1 = sieve (gen_list 2) [] = [ 2 ]

let sieve_test2 = sieve (gen_list 4) [] = [ 3; 2 ]

let sieve2_test1 = sieve2 [ 2 ] = [ 2 ]

let sieve2_test2 = sieve2 [ 2; 3; 4 ] = [ 2; 3 ]

let two_to_n n =
  let rec loop i = if i > n then [] else i :: loop (i + 1) in
  loop 2

let two_to_n_test1 = two_to_n 2 = [ 2 ]

let two_to_n_test2 = two_to_n 1 = []

let two_to_n_test3 = two_to_n 5 = [ 2; 3; 4; 5 ]

let prime n = sieve2 (two_to_n n)

let prime_test1 = prime 2 = [ 2 ]

let prime_test2 = prime 1 = []

let prime_test3 = prime 4 = [ 2; 3 ]
