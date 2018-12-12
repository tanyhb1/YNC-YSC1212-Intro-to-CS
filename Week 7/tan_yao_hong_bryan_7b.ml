(*TAN YAO HONG BRYAN ASSIGNMENT NO. 7b *)

(* question 1 w/ extra credit *)

let rec fold_left f b l =
  match l with
  | [] -> b
  | x :: xs -> fold_left f (f x b) xs;;

let rec fold_right f b l =
  match l with
  | [] -> b
  | x :: xs -> f x (fold_right f b xs);;

let flatten l =
  fold_left (fun a b -> fold_right (fun x y -> x::y) a b) [] l;;

(* solution is in o(n) because in the list of list, fold_left takes 2 lists and applies fold_right to them. fold_right takes the two lists and prepends (::) each element starting from the last element  in the first list to the second list. After all the elements in the first list are prepended, this whole list is now the accumulator, and will be the second list in the next fold_left, and so on. Thus, it prepends a total of n where n is the number of elements in the whole list of lists. *)

(* extra credit : it is also tail-recursive, since fold_left and fold_right are tail recursive. *)

(* question 2 *)

type 'a set =
  | Nil
  | Cons of 'a * 'a set;;

let empty_set = Nil;;

let singleton x = Cons(x, Nil);;

let element_of x s =
  let rec element_of_helper x s =
    match s with
    | Nil -> false
    | Cons(h, t) -> if x = h then true else element_of_helper x t
  in element_of_helper x s;;

let union s1 s2 =
  let rec union_helper s1 s2 =
    match s1 with
    | Nil -> s2
    | Cons(h, t) -> if element_of h s2 then union_helper t s2
                    else Cons(h, union_helper t s2)
  in union_helper s1 s2;;


let intersection s1 s2 =
  let rec intersection_helper s1 s2 =
    match s1 with
    | Nil -> Nil
    | Cons(h, t) -> if element_of h s2 then Cons(h, intersection_helper t s2)
                    else intersection_helper t s2
  in intersection_helper s1 s2;;

(* question 3 *)

type 'a tree =
  | Lf
  | Br of ('a tree) * 'a * ('a tree)

let tree_map f t =
  let rec tree_map_helper f t =
    match t with
    | Lf -> Lf
    | Br(left, value, right) -> Br(tree_map_helper f left, f value, tree_map_helper f right)
  in tree_map_helper f t;;

(* question 4 w/ extra-credit*)

let listify t =
  let rec listify_helper t acc =
    match t with
    | Lf -> acc
    | Br(left, value, right) -> listify_helper left (value :: (listify_helper right acc))
  in listify_helper t [];;

 

(* question 4 extra-credit *)
(* my implementation of listify traverses each element in the ordered tree exactly once. it recursively calls itself until, in the final recursive call, it reaches the right-most branch which has the greatest value. then it 'backtracks' by completing pending recursive calls in the stack, which will, from the previous branch, go left instead of right. Through this stack of pending recursive calls, listify will go to each element exactly once, and thus my solution is O(n). 

another way I thought about how I could explain why my solution is O(n) is because listify makes two deep recursive calls that go all the way to the height of the ordered/balanced tree, where height = log n. Thus, time complexity of the program is doubled each time listify is called, which can be represented as 2**n. However, since the property of an ordered tree is that its height is logn, listify is called only log n times, and thus log (base 2) (2 ** n) = n log (base 2) 2 = n, and listify is O(n). Another way to express this would be that the time complexity doubles with each recursive call, but is at the same time halved because of the ordered tree, and thus O(n).*)
