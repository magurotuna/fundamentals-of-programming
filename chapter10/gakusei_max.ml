type gakusei_t = { name : string; score : int; grade : string }

let maguro = { name = "maguro"; score = 50; grade = "D" }

let tuna = { name = "tuna"; score = 60; grade = "C" }

let alice = { name = "alice"; score = 80; grade = "A" }

let bob = { name = "bob"; score = 70; grade = "B" }

let bottom = { name = ""; score = min_int; grade = "" }

(* gakusei_t -> gakusei_t -> bool *)
let former_is_greater a b = a.score >= b.score

(* gakusei_t list -> gakusei_t *)
let rec gakusei_max lst =
  match lst with
  | [] -> bottom
  | first :: rest ->
      let max_rest = gakusei_max rest in
      if former_is_greater first max_rest then first else max_rest

let t1 = gakusei_max [] = { name = ""; score = min_int; grade = "" }

let t2 = gakusei_max [ maguro ] = maguro

let t3 = gakusei_max [ maguro; tuna ] = tuna

let t4 = gakusei_max [ maguro; maguro; alice; bob ] = alice
