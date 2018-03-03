(*Class 6a *)

(*in which we went through certain ways to analyze the running time of a queue, specifically amortized analysis of the optimized en/de-queue *)

(*chapter 7 of textbook *)

let dict = [(1,2); (3,4); (5,6)];;

let rec lookup x l =
  match l with
  | [] -> raise Not_found
  | (k, v) :: t -> if k = x then v else lookup x t;;

let rec add k v l =
  match l with
  | [] -> [(k, v)]
  | (a, b) :: t -> if a = k then (k, v) :: t else (a, b) :: add k v t;;

let rec remove k l =
  match l with
  | [] -> []
  | (a, b) :: t -> if a = k then t else (a, b) :: remove k t;;

let rec key_exists k l =
  try
    let _ = lookup k l in true
  with
    Not_found -> false;;

(*question 1 *)

let no_keys l =
  let helper l acc =
    match l with
    | [] -> acc
    | (a, b) :: t -> no_keys t (acc + 1)
  in helper l 0;;

(* question 2 *)

let rec replace k v l =
  match l with
  | [] -> raise Not_found
  | (a, b) :: t -> if a = k then (k, v) :: t else replace k v t;;

(* question 3 *)

let rec mkdict keys values =
  match keys, values with
  | [], [] -> []
  | _, [] -> raise (Invalid_argument "mkdict")
  | [], _ -> raise (Invalid_argument "mkdict")
  | k::kt, v::vt -> (k,v)::mkdict kt vt;;

(*question 4 *)

let decon l =
  let rec helper l keyl valuel =
    match l with
    | [] -> (keyl, valuel)
    | (a, b) :: t -> helper t (a::keyl) (b::valuel)
  in helper l [] [];;

(* question 5 *)
let rec member x l =
  match l with
  | [] -> false
  | h :: t -> if h = x then true else member x t;;

let rec mkdict2_helper seen l =
  match l with
  | [] -> []
  | (a, b) :: t -> if member a seen then mkdict2_helper seen t
                   else (a, b) :: mkdict2_helper (a::seen) t;;
let mkdict2 l =
  mkdict2_helper [] l;;

(*question 6*)

let union a b =
  let rec helper a b lst =
    match a, b with
    | [], [] -> []
    | [], (bk, bv) :: bt -> if key_exists bk lst then helper [] bt lst else helper [] bt ((bk,bv)::lst)
    | (ak, av) :: at, (bk, bv) :: bt -> helper at b ((ak,av)::lst)
  in helper a b [];;

(*chapter 9*)
(*question 1*)
(*the function g a b c has type a`->b`->c`->y which can also be written as a`->(b`->(c`->y)). Thus, it takes an argument of a` and returns a function of (b` -> (c`->y)) which when given b` will return a function of (c` -> y) which when given an argument c` will return y. When we write let g a b c = ... this is just short hand for fun a -> fun b -> fun c -> ... *)

(*question 2*)
(*type a' -> a' list -> true
  type a' -> a' list *)

(*question 3*)
let rec map f l =
  match l with
  | [] -> []
  | h :: t -> f h :: map f t;;

let div b a =
  a / b;;

let rec truncate n l =
  match l with
  | [] -> []
  | h :: t -> ;;

(*chapter 10*)

type colour = Red | Green | Blue | Yellow;;
let col = Blue;;
let cols = [Red; Red; Green; Yellow];;
let colpair = ('R', Red);;

type colour =
  |Red
  |Green
  |Blue
  |Yellow
  |RGB of int * int * int;;

let cols = [Red; Red; Green; Yellow; RGB (150, 0, 255)];;

let components c =
  match c with
  | Red -> (255, 0, 0)
  | Green -> (0, 255, 0)
  | Blue -> (0, 0, 255)
  | Yellow -> (255, 255, 0)
  | RGB (r,g,b) -> (r,g,b);;

(* where None becomes an object of type 'a option *)
type 'a option = None | Some of 'a;;

let nothing = None;;

let number = Some 50;;

let numbers = [Some 12; None; None; Some 2];;

let rec lookup_opt x l =
  match l with
  | [] -> None
  | (k, v)::t -> if k = x then Some v else lookup_opt x t;;

(* deconstructing a list *)
type 'a sequence = Nil | Cons of 'a * 'a sequence;;

(* for lists, *)
let rec length l =
  match l with
  | [] -> 0
  | _::t -> 1 + length t;;

let rec append a b =
  match a with
  | [] -> b
  | h :: t -> h :: append t b;;

(* for decon *)
let rec length' s =
  match s with
  | Nil -> 0
  | Cons (_, t) -> 1 + length' t;;

let rec append' c d =
  match c with
  | Nil -> d
  | Cons (h, t) -> Cons (h, append' t d);;

(* deconstructing mathematical operations *)
type expr =
  | Num of int
  | Add of expr * expr
  | Subtract of expr * expr
  | Multiply of expr * expr
  | Divide of expr * expr;;

Add (Num 1, Multiply (Num 2, Num 3));;

let rec evaluate e =
  match e with
  | Num x -> x
  | Add (e, e') -> evaluate e + evaluate e'
  | Subtract (e, e') -> evaluate e - evaluate e'
  | Multiply (e, e') -> evaluate e * evaluate e'
  | Divide (e, e') -> evaluate e / evaluate e';;

(*question 1 *)
type rect =
  | Square of int
  | Rectangle of int * int;;

(*question 2*)

let area o =
  match o with
  | Square x -> x * x
  | Rectangle (x, y) -> x * y;;

(*question 3*)

let rotate o =
  match o with
  | Square x -> o
  | Rectangle (x, y) -> if x < y then Rectangle (y, x) else o;;

(*question 4*)

let width_of_rect r =
  match r with
  | Square x -> x
  | Rectangle (w, _) -> w;;

let rect_compare a b =
  width_of_rect a > width_of_rect b;;

let pack rects =
  sort rect_compare (map rotate rects);;

(*question 5*)
type 'a sequence = Nil | Cons of 'a * 'a sequence;;

let rec take n s =
  if n = 0 then Nil else
  match s with
  | Nil -> raise (Invalid_argument "take")
  | Cons (a, b) -> Cons(a, take (n-1) b);;

let rec drop n s =
  if n = 0 then s else
  match s with
  | Nil -> raise (Invalid_argument "drop")
  | Cons (a, b) -> drop (n-1) b;;

let rec map f s =
  match s with
  | Nil -> Nil
  | Cons (a, b) -> Cons (f a, map f b);;

(*question 6*)

type expr =
  | Num of int
  | Add of expr * expr
  | Subtract of expr * expr
  | Multiply of expr * expr
  | Divide of expr * expr
  | Exponential of expr * expr;;

Add (Num 1, Multiply (Num 2, Num 3));;

let rec apply f n x =
  if n = 0 then x
  else f (apply f (n-1) x);;

let power a b =
  apply (fun x -> x * a) b 1;;
let rec evaluate e =
  match e with
  | Num x -> x
  | Add (e, e') -> evaluate e + evaluate e'
  | Subtract (e, e') -> evaluate e - evaluate e'
  | Multiply (e, e') -> evaluate e * evaluate e'
  | Divide (e, e') -> evaluate e / evaluate e'
  | Exponential (e, e') -> power (evaluate e) (evaluate e');;
