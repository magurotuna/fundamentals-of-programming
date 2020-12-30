(* string list -> string *)
let concat lst = List.fold_right ( ^ ) lst ""

let test1 = concat [] = ""

let test2 = concat [ "a" ] = "a"

let test3 = concat [ "" ] = ""

let test4 = concat [ "maguro"; "tuna" ] = "magurotuna"
