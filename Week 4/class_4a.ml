(* Class 4a *)

let rec doublelist l =
  match l with
  | [] -> []
  | h :: t -> 2 * h :: doublelist t;;

let rec toASCII l =
  match l with
  | [] -> []
  | h :: t -> int_of_char h :: toASCII t ;;

(* 'a -> 'b refers to the general application of f unto h *)
(* this is a general function to map f unto elements of the list *)
(* further abstraction of ^^ *)

let rec map f l =
  match l with
  | [] -> []
  | h :: t -> (f h) :: map f t;;

let double x = 2 * x;;
double 12;;

map double [5;3;1];;
map int_of_char ['h';'e';'l';'l';'o'];;

let rec filter tester lst =
  match lst with
  | [] -> []
  | h :: t -> if tester h then h :: filter tester t else filter tester t;;

let lowercase c =
  int_of_char c >= 97 && int_of_char c <= 122;;

filter lowercase ['h';'e';'L';'L';'o'];;

(* quicksort algorithm *)

let rec partition_lower p l =
  match l with
  | [] -> []
  | h :: t -> if h <= p then h :: partition_lower p t else partition_lower p t;;

let rec partition_upper p l =
  match l with
  | [] -> []
  | h :: t -> if h > p then h :: partition_upper p t else partition_upper p t;;

let rec quicksort l =
  match l with
  | [] -> []
  | p :: t -> let lowers = partition_lower p t in
              let uppers = partition_upper p t in
              let lowers_sorted = quicksort lowers in
              let uppers_sorted = quicksort uppers in
              lowers_sorted @ (p :: uppers_sorted);;

let rec length l = match l with [] -> 0 | _ :: t -> 1 + length t ;;
let rec quickselect l n =
  match l with
  | [] -> (-1)
  | p :: t - > if n = 0 then p else
               let lowers = partition_lower p t in
               let uppers = partition_upper p t in
               let length_lowers = length lowers in
               if inthelowerhalf then quickselect lowers n else quickselect uppers (n - length_lowers);;



(* Chapter 6 *)

let rec map f l =
  match l with
  | [] -> []
  | h :: t ->  f h :: map f t;;

                                    
let halve x = x/2;;

map halve [10;8;6;4;2];;

(* question 1 *)

let rec calm l =
  match l with
  | [] -> []
  | '!':: t -> '.' :: calm t
  | h :: t ->  h ::calm t;;


let calm2 l =
  map (fun x -> match x with '!' -> '.' | _ -> x) l;;

let clip i =
  if i <= 0 then 1
  else if i <= 10 then i
  else 10;;

let cliplist l =
  map clip l;;

let cliplist2 l =
  map (fun x -> if x <= 0 then 1 else if x <= 10 then x else 10) l;;

let rec apply f n l =
  if n = 0 then l else f (apply f (n - 1) l);;

let rec filter f l =
  match l with
  | [] -> []
  | h :: t -> if f h then h :: filter f t else filter f t;;

let rec for_all f l =
  match l with
  | [] -> true
  | h :: t -> f h && for_all f t;;

let rec map1 f l =
  match l with
  | [] -> []
  | x :: xss -> map f x ::  map1 f xss;;
