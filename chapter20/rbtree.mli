module type Tree_t = sig
  (* key: 'a, value: 'b *)
  type ('a, 'b) t

  (* empty tree *)
  val empty : ('a, 'b) t

  (* inserts key and value to the tree, and returns the result *)
  val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t

  (* searches for the key in the tree, and returns the matching value *)
  (* if not found, raises `Not_found` exception *)
  val search : ('a, 'b) t -> 'a -> 'b
end
