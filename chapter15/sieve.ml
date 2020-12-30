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

let sieve_test1 = sieve (gen_list 2) [] = [ 2 ]

let sieve_test2 = sieve (gen_list 4) [] = [ 3; 2 ]
