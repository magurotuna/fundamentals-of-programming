type person_t = {
  name : string;
  height : int;
  weight : int;
  birth_month : int;
  birth_date : int;
  blood : string;
}

let p1 =
  {
    name = "magurotuna";
    height = 170;
    weight = 70;
    birth_month = 9;
    birth_date = 20;
    blood = "A";
  }

let p2 =
  {
    name = "foo";
    height = 180;
    weight = 60;
    birth_month = 12;
    birth_date = 30;
    blood = "AB";
  }

let p3 =
  {
    name = "bar";
    height = 180;
    weight = 60;
    birth_month = 12;
    birth_date = 30;
    blood = "O";
  }

let lst1 = []

let lst2 = [ p1 ]

let lst3 = [ p1; p2 ]

let lst4 = [ p1; p2; p3 ]

(* person_t list -> int *)
let rec count_ketsueki_A lst =
  match lst with
  | [] -> 0
  | { blood = b } :: rest ->
      if b = "A" then 1 + count_ketsueki_A rest else count_ketsueki_A rest

let t1 = count_ketsueki_A lst1 = 0

let t2 = count_ketsueki_A lst2 = 1

let t3 = count_ketsueki_A lst3 = 1

let t4 = count_ketsueki_A lst4 = 1
