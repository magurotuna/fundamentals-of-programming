type tree_t = Empty | Leaf of int | Node of tree_t * int * tree_t

let tree1 = Empty

let tree2 = Leaf 2

let tree3 = Leaf 42

let tree4 = Node (tree2, 7, tree1)

let tree5 = Node (tree4, 9, tree3)

(* tree_t -> tree_t *)
let rec tree_double tree =
  match tree with
  | Empty -> Empty
  | Leaf n -> Leaf (2 * n)
  | Node (left, n, right) -> Node (tree_double left, 2 * n, tree_double right)

let test1 = tree_double tree1 = tree1

let test2 = tree_double tree2 = Leaf 4

let test3 = tree_double tree3 = Leaf 84

let test4 = tree_double tree4 = Node (Leaf 4, 14, Empty)

let test5 = tree_double tree5 = Node (Node (Leaf 4, 14, Empty), 18, Leaf 84)

(* (int -> int) -> tree_t -> tree_t *)
let rec tree_map f tree =
  match tree with
  | Empty -> Empty
  | Leaf n -> Leaf (f n)
  | Node (left, n, right) -> Node (tree_map f left, f n, tree_map f right)

let tt1 = tree_map (fun x -> x * 2) tree1 = tree1

let tt2 = tree_map (fun x -> x * 2) tree2 = Leaf 4

let tt3 = tree_map (fun x -> x * 2) tree4 = Node (Leaf 4, 14, Empty)

(* tree_t -> int *)
let rec tree_length tree =
  match tree with
  | Empty -> 0
  | Leaf n -> 1
  | Node (left, n, right) -> tree_length left + 1 + tree_length right

let ttt1 = tree_length tree1 = 0

let ttt2 = tree_length tree2 = 1

let ttt3 = tree_length tree3 = 1

let ttt4 = tree_length tree4 = 2

let ttt4 = tree_length tree5 = 4

(* tree_t -> int *)
let rec tree_depth tree =
  match tree with
  | Empty -> 0
  | Leaf n -> 0
  | Node (left, n, right) ->
      let ld = tree_depth left in
      let rd = tree_depth right in
      1 + max ld rd

let tttt1 = tree_depth tree1 = 0

let tttt2 = tree_depth tree2 = 0

let tttt3 = tree_depth tree3 = 0

let tttt4 = tree_depth tree4 = 1

let tttt5 = tree_depth tree5 = 2
