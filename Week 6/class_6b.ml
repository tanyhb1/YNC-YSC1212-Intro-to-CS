(*Class 6b *)

type color = Red | Green | Blue;;

type fancycolor = Red of int | Green of int | Blue of int;;

Red 52;;

match Red 24 with
| Green x -> x + 1
| Blue x -> x -50
| Red x -> x + x;;

type superfancycolor = Red of int list | Green of int * int | Blue of int * char;;

Red [2;3;4];;
Green (5,7);;
Blue (7, 'x');;

type intoption = None | Some of int;;

let head l =
  match l with
  | h :: _ -> Some h
  | [] -> None;;

type 'a option = None | Some of 'a;;

let head l =
  match l with
  | h :: _ -> Some h
  | [] -> None;;

type intlist = Nil | Cons of int * intlist;;
let aintlist = Cons (3, (Cons (1, (Cons (4, (Cons (1, Nil)))))));;

type 'a tree = Leaf | Node of 'a * ('a tree) * ('a tree);;

type ('a,'b) fancytree =
  | Leaf of 'b
  | Node of (('a, 'b) fancytree) * 'a * (('a, 'b) fancytree);;

let afancytree = Node (Node (Leaf true, 17, Leaf false),
                       15,
                       Leaf true);;


type 'a tree = Leaf | Node of ('a tree) * 'a  * ('a tree);;
let rec find k t =
  match t with
  | Leaf -> raise Not_found
  | Node (left, (key, value), right) ->
     if key = k then value
     else if k < key then find k left
     else find k right;;

let rec insert k v t =
  match t with
  | Leaf -> Node (Leaf, (k,v), Leaf)
  | Node (left, (key, value), right) ->
     if k < key then Node (insert k v left , (key, value), right)
     else if k > key then Node (left, (key, value), insert k v right)
     else Node (left, (k, v), right);;

let emptydict = Leaf;;

let d1 = insert 8 "Hello" emptydict;;
let d2 = insert 88 "World" d1;;
let d3 = insert 4 "Homejoy" d2;;
let d4 = insert 44 "Final exam" d3;;

find 44 d4;;
find 88 d4;;
