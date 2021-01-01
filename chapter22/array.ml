(* int array -> int array *)
let fib_array arr =
  let len = Array.length arr in
  (* int -> unit *)
  let rec loop i =
    if i >= len then ()
    else (
      if i = 0 then arr.(i) <- 0
      else if i = 1 then arr.(i) <- 1
      else arr.(i) <- arr.(i - 1) + arr.(i - 2);
      loop (i + 1))
  in
  loop 0;
  arr

let result = fib_array [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]

let test1 = result = [| 0; 1; 1; 2; 3; 5; 8; 13; 21; 34 |]
