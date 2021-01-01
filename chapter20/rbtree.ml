type color_t = Red | Black

type ('a, 'b) t = Empty | Node of ('a, 'b) t * 'a * 'b * color_t * ('a, 'b) t

let empty = Empty

let test_data1 = Node (empty, 1, "a", Red, empty)

let test_data2 = Node (test_data1, 2, "b", Red, empty)

let test_data3 = Node (test_data2, 3, "c", Black, empty)

let unbalanced = test_data3

let x = Node (empty, 1, "a", Black, empty)

let z = Node (empty, 3, "c", Black, empty)

let y = Node (x, 2, "b", Red, z)

let balanced = y

(* rb_tree_t -> rb_tree_t *)
let balance tree =
  match tree with
  | Node (* pattern 1 *)
      ( Node (Node (a, x_key, x_val, Red, b), y_key, y_val, Red, c),
        z_key,
        z_val,
        Black,
        d )
  | Node (* pattern 2 *)
      ( Node (a, x_key, x_val, Red, Node (b, y_key, y_val, Red, c)),
        z_key,
        z_val,
        Black,
        d )
  | Node (* pattern 3 *)
      ( a,
        x_key,
        x_val,
        Black,
        Node (Node (b, y_key, y_val, Red, c), z_key, z_val, Red, d) )
  | Node (* pattern 4 *)
      ( a,
        x_key,
        x_val,
        Black,
        Node (b, y_key, y_val, Red, Node (c, z_key, z_val, Red, d)) ) ->
      Node
        ( Node (a, x_key, x_val, Black, b),
          y_key,
          y_val,
          Red,
          Node (c, z_key, z_val, Black, d) )
  | _ -> tree

let balance_test1 = balance unbalanced = balanced

(* ('a, 'b) rb_tree_t -> ('a, 'b) rb_tree_t *)
let change_black tree =
  match tree with
  | Empty -> Empty
  | Node (l, k, v, _, r) -> Node (l, k, v, Black, r)

(* ('a, 'b) rb_tree_t -> 'a -> 'b -> ('a, 'b) rb_tree_t *)
let insert tree key value =
  let rec inner_insert tree key value =
    match tree with
    | Empty -> Node (Empty, key, value, Red, Empty)
    | Node (left, n_key, n_value, color, right) ->
        if n_key = key then Node (left, n_key, value, color, right)
        else if key < n_key then
          let inserted = inner_insert left key value in
          balance (Node (inserted, n_key, n_value, color, right))
        else
          let inserted = inner_insert right key value in
          balance (Node (left, n_key, n_value, color, inserted))
  in
  let result = inner_insert tree key value in
  match result with
  | Empty ->
      assert false
      (* `result` is never `Empty` because it is the result of inserting some element. *)
  | _ -> change_black result

let insert_test1 = insert empty 1 "a" = Node (empty, 1, "a", Black, empty)

let insert_test2 =
  let a = insert empty 1 "a" in
  let ab = insert a 2 "b" in
  let abc = insert ab 3 "c" in
  let balanced_root_black = change_black balanced in
  abc = balanced_root_black

let rec depth tree =
  match tree with
  | Empty -> 0
  | Node (left, _, _, _, right) -> 1 + max (depth left) (depth right)

let gen_tree lst =
  let f tree key_value =
    match key_value with key, value -> insert tree key value
  in
  List.fold_left f empty lst

let tree6 =
  gen_tree [ (1, "a"); (2, "b"); (3, "c"); (4, "d"); (5, "e"); (6, "f") ]

let tree7 =
  gen_tree
    [ (1, "a"); (2, "b"); (3, "c"); (4, "d"); (5, "e"); (6, "f"); (7, "g") ]

let rbtree_works_1 = depth tree6 = 4

let rbtree_works_2 = depth tree7 = 3

let rec search tree key =
  match tree with
  | Empty -> raise Not_found
  | Node (left, n_key, n_val, _, right) ->
      if n_key = key then n_val
      else if key < n_key then search left key
      else search right key

let search_test1 = search tree6 1 = "a"

let search_test2 = search tree6 6 = "f"

let search_test3 =
  (try search tree6 7 with Not_found -> "Not_found") = "Not_found"
