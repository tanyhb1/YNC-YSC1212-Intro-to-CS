(* Class 3b *)

let rec merge lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h1 :: t1 -> match lst2 with
                | [] -> lst1
                | h2 :: t2 -> if h1 < h2 then h1 :: merge t1 lst2
                              else h2 :: merge lst1 t2;;
let rec msort lst =
  match lst with
  | [] -> []
  | [_] -> lst
  (* take any list at all *)
  | _ -> let n = (length lst)/2 in
         let left = take n lst in
         let right = drop n lst in
         merge (msort left) (msort right);;
         

let rec length l =
  match l with
  | [] -> 0
  | h :: t -> 1 + length t;;
let rec drop n l =
  if n = 0 then l else
    match l with h :: t -> drop (n-1) t
               | [] -> [];;
let rec take n l =
  if n = 0 then [] else
    match l with h :: t -> h:: (take (n-1) t)
               | [] -> [];;

