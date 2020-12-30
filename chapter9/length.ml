let rec length lst = match lst with [] -> 0 | first :: rest -> 1 + length rest

let t1 = length [] = 0

let t2 = length [ 1 ] = 1

let t3 = length [ 1; 2 ] = 2

let t4 = length [ 1; 2; 3 ] = 3
