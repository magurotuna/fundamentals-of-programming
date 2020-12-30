(* string list -> string *)
let rec concat lst =
  match lst with [] -> "" | first :: rest -> first ^ concat rest

let t1 = concat [] = ""

let t2 = concat [ "a" ] = "a"

let t3 = concat [ "a"; "b" ] = "ab"
