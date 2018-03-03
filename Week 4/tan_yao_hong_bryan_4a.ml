(* TAN YAO HONG BRYAN ASSIGNMENT NO. 4a *)

(* question 1 *)

let rec meld f b l =
  match l with
  | [] -> b
  | x :: xs -> f x (meld f b xs);;

(* question 2 *)

let rec meld_left f b l =
  match l with
  | [] -> b
  | x :: xs -> meld_left f (f b x) xs;;

(* question 3 *)

let rec createList n start =
  if n < 0 || start > n then [] else start :: createList n (start + 1);;

let rec filter f l =
  match l with
  | [] -> []
  | h :: t -> if f h then h :: filter f t
              else filter f t;;

let rec notMultiple l =
  match l with
  | [] -> []
  | h :: t -> h :: (notMultiple (filter (fun a -> a mod h <> 0) t));;
            
let rec findPrimes n =
  if n < 2 then []
  else let lst = createList n 2 in
       notMultiple lst;;


(* question 4 *)

let rec strange_type x =
  strange_type x;;
