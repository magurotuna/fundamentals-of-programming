(* int list -> int *)
let rec sum lst = match lst with [] -> 0 | first :: rest -> first + sum rest

(* tests *)
let test1 = sum [] = 0

let test2 = sum [ 1 ] = 1

let test3 = sum [ 2 ] = 2

let test4 = sum [ 1; 2 ] = 3

let test5 = sum [ 0; 2 ] = 2

let test6 = sum [ 0; 1; 2 ] = 3

let test7 = sum [ 0; 2; 5; 42 ] = 49
