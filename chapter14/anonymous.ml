;;
(fun x -> (x * x) - 1) 3

let even lst = List.filter (fun x -> x mod 2 = 0) lst

let test1 = even [] = []

let test2 = even [ 0 ] = [ 0 ]

let test3 = even [ 1 ] = []

let test4 = even [ 0; 2; 3 ] = [ 0; 2 ]
