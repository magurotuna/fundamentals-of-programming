let rec fold_right f lst init =
  match lst with
  | [] -> init
  | first :: rest -> f first (fold_right f rest init)

(* strint list -> string *)
let concat lst =
  let concat_sub s1 s2 = s1 ^ s2 in
  fold_right concat_sub lst ""

let test1 =
  concat [ "spring"; "summer"; "autumn"; "winter" ] = "springsummerautumnwinter"

let test2 = concat [] = ""

let test3 = concat [ "foo" ] = "foo"

type gakusei_t = { name : string; score : int; grade : string }

let maguro = { name = "maguro"; score = 50; grade = "D" }

let tuna = { name = "tuna"; score = 60; grade = "C" }

let alice = { name = "alice"; score = 80; grade = "A" }

let bob = { name = "bob"; score = 70; grade = "B" }

(* galusei_t list -> int *)
let gakusei_sum lst =
  let sum_score x y = x.score + y in
  fold_right sum_score lst 0

let t1 = gakusei_sum [] = 0

let t2 = gakusei_sum [ maguro ] = 50

let t3 = gakusei_sum [ maguro; tuna ] = 110

let t4 = gakusei_sum [ alice; maguro ] = 130
