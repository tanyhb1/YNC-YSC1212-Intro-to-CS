(* class 4b *)

exception My_exn;;

raise My_exn;;

let head l =
  match l with
  | h :: _ -> h
  | [] -> raise Not_found;;

exception Head_not_found;;


let head l =
  match l with
  | h :: _ -> h
  | [] -> raise Head_not_found;;

let test x = raise Not_found;;

(test 17) + 5;;

let head l =
  match l with
  | h :: _ -> h;;

exception Myexn of int;;
let strange l =
  match l with
  | h1 :: h2 :: _ -> h1 + h2
  | h1 :: [] -> raise (Myexn 0)
  | [] -> raise (Myexn 1);;

let strange2 x =
  if x mod 2 = 0 then x + x
  else raise (Myexn x);;


(* chapter 7 *)

let rec take n l =
  match l with
  | [] -> if n = 0 then [] else raise (Invalid_argument "take")
  | h :: t -> if n < 0 then raise (Invalid_argument "take") else
                if n = 0 then [] else h :: take (n - 1) t ;;

let rec drop n l =
  match l with
  | [] -> if n = 0 then [] else raise (Invalid_argument "drop")
  | h :: t -> if n < 0 then raise (Invalid_argument "drop") else
                if n = 0 then t else drop (n - 1) t;;

exception Problem;;
exception NotPrime of int;;

(* exception handlers *)

let safe_divide x y =
  try x / y with
    Division_by_zero -> 0;;

let rec last l =
  match l with
  | [] -> raise Not_found
  | [x] -> [x]
  | _ :: t -> last t;;

(* question 1 *)

let rec smallest_inner current found l =
  match l with
  | [] -> if found then current else raise Not_found
  | h :: t -> if h > 0 && h < current
              then smallest_inner h true t
              else smallest_inner current found t;;

let smallest l =
  smallest_inner max_int false l;;
                     


(*question 2 *)

let smallest_or_zero l =
  try smallest l with
    Not_found -> 0;;

(*question 3*)

exception Negative_argument;;

let rec helper x n =
  if x * x > n then (x - 1) else helper (x + 1) n;;
let sqrt n =
  if n < 0 then raise Negative_argument else helper 1 n;;

(*question 4*)
let sqrt1 n =
  try sqrt n with
    Negative_argument -> 0;;
