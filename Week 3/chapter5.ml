
let rec merge x y =
  match x, y with
  | [], l -> l
  | l, [] -> l
  | hx :: tx, hy :: ty -> if hx < hy then hx :: merge tx (hy :: ty)
                          else hy :: merge (hx :: tx) ty;;

let rec take n l =
  if n = 0 then [] else
    match l with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs;;

let rec drop n l =
  if n = 0 then l else
    match l with
    | [] -> []
    | x :: xs -> drop (n - 1) xs;;

let length l =
  let rec helper l acc = 
    match l with
    | [] -> acc
    | x :: xs -> helper xs (acc + 1)
  in helper l 0;;


let rec msort l =
  match l with
  | [] -> []
  | [x] -> [x]
  | _ ->
     let len = (length l)/2 in
     let left = take len l in
     let right = drop len l in
     merge (msort left) (msort right);;

let rec insert x l =
  match l with
  | [] -> [x]
  | h :: t -> if x <= h then x :: (h :: t)
              else h :: (insert x t);;
let rec rev_insert x l =
  match l with
  | [] -> [x]
  | h :: t -> if x >= h then x :: (h :: t)
              else h :: (insert x t);;
let rec sort l =
  match l with
  | [] -> []
  | x :: xs -> insert x (sort xs);;

let rec rev_sort l =
  match l with
  | [] -> []
  | x :: xs -> rev_insert x (sort xs);;

let rec checker l =
  match l with
  | [] -> true
  | _ :: [] -> true
  | h :: m :: t ->  h <= m && checker (m::t);;

let rec sort l =
  match l with
  | [] -> []
  | h :: t -> insert h (sort t);;

let rec insert x l =
  match l with
  | [] -> []
  | [x] -> [x]
  | h :: t -> if x < h then x :: h :: t
              else h :: insert x t;;

let rec sort_combined l =
  let rec helper_insert x l =
    match l with
    | [] -> []
    | [x] -> [x]
    | h :: t -> if x < h then x :: h :: t
                else h :: helper_insert x t
  in
  match l with
  | [] -> []
  | h :: t -> helper_insert h (sort_combined t);;
           


