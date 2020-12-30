type gakusei_t = { name : string; score : int; grade : string }

(* gakusei_t list -> gakusei_t -> gakusei_t list *)
let rec insert sorted_lst gakusei =
  match sorted_lst with
  | [] -> [ gakusei ]
  | first :: rest ->
      if gakusei.score <= first.score then gakusei :: sorted_lst
      else first :: insert rest gakusei

let maguro = { name = "maguro"; score = 50; grade = "D" }

let tuna = { name = "tuna"; score = 60; grade = "C" }

let alice = { name = "alice"; score = 80; grade = "A" }

let bob = { name = "bob"; score = 70; grade = "B" }

let input_lst = [ maguro; bob; alice ]

let output_lst = [ maguro; tuna; bob; alice ]

let t0 = insert input_lst tuna = output_lst

let t1 = insert [] maguro = [ maguro ]

let t2 = insert [ maguro ] maguro = [ maguro; maguro ]

let t3 = insert [ maguro ] tuna = [ maguro; tuna ]

(* insertion sort for gakusei_t *)
(* gakusei_t list -> gakusei_t list *)
let rec ins_sort_gakusei lst =
  match lst with
  | [] -> []
  | first :: rest -> insert (ins_sort_gakusei rest) first

let tt0 = ins_sort_gakusei [] = []

let tt1 = ins_sort_gakusei [ maguro ] = [ maguro ]

let tt2 = ins_sort_gakusei [ tuna; maguro ] = [ maguro; tuna ]
