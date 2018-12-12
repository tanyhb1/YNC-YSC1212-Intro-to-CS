(* week-10a_fold-right-for-binary-trees.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2017-2018 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Version of Mon 19 Mar 2018 *)

(* ********** *)

type 'a binary_tree =
  | Leaf of 'a
  | Node of 'a binary_tree * 'a binary_tree;;

let fold_right_binary_tree leaf_case node_case t_init =
 (* fold_right_binary_tree : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'a binary_tree -> 'b *)
  let rec climb t =
    match t with
    | Leaf v ->
       leaf_case v
    | Node (t1, t2) ->
       let c1 = climb t1
       and c2 = climb t2
       in node_case (c1, c2)
  in climb t_init;;

(* ********** *)

let show_int n =
  if n < 0
  then "(" ^ string_of_int n ^ ")"
  else string_of_int n;;

let test_show_binary_tree_int candidate =
 (* test_show_binary_tree_int : (int binary_tree -> string) -> bool *)
     (candidate show_int (Leaf 0)
      = "Leaf 0")
  && (candidate show_int (Node (Leaf 1, Leaf 10))
      = "Node (Leaf 1, Leaf 10)")
  && (candidate show_int (Node (Leaf 1, Node (Leaf 10, Leaf 100)))
      = "Node (Leaf 1, Node (Leaf 10, Leaf 100))")
  && (candidate show_int (Node (Node (Leaf 1, Leaf 10), Leaf 100))
      = "Node (Node (Leaf 1, Leaf 10), Leaf 100)")
  && (candidate show_int (Node (Node (Leaf 1, Leaf 10), Node (Leaf 100, Leaf 1000)))
      = "Node (Node (Leaf 1, Leaf 10), Node (Leaf 100, Leaf 1000))")
  (* etc. *);;

let show_binary_tree show_yourself t_init =
  let rec visit t =
    match t with
    | Leaf v ->
       "Leaf " ^ (show_yourself v)
    | Node (t1, t2) ->
       let c1 = visit t1
       and c2 = visit t2
       in "Node (" ^ c1 ^ ", " ^ c2 ^ ")"
  in visit t_init;;

let () = assert (test_show_binary_tree_int show_binary_tree);;

(* Exercise 1:

   Express show_binary_tree using fold_right_binary_tree
   and verify that it passes the unit test.
*)


let show_binary_tree_alt show_yourself t_init =
  fold_right_binary_tree (fun a -> "Leaf " ^ (show_yourself a))
    (fun (c1,c2) -> "Node (" ^ c1 ^ ", " ^ c2 ^ ")")
    t_init;;


let () = assert (test_show_binary_tree_int show_binary_tree_alt);;

(* ********** *)
let test_number_of_leaves candidate =
     (candidate (Leaf 10) = 1)
  && (candidate (Node (Leaf 33,
                       Leaf 44)) = 2)
  && (candidate (Node (Node (Leaf 33,
                             Leaf 34),
                       Leaf 44)) = 3)
  && (candidate (Node (Node (Leaf 33,
                             Node (Leaf 34,
                                   Leaf 35)),
                       Node (Leaf 44,
                             Leaf 45))) = 5)
  (* etc. *);;

let number_of_leaves t_init =
  let rec count t =
    match t with
    | Leaf _ ->
       1
    | Node (t1, t2) ->
       let c1 = count t1
       and c2 = count t2
       in c1 + c2
  in count t_init;;

let () = assert (test_number_of_leaves number_of_leaves);;

(* Exercise 2:

   Express number_of_leaves using fold_right_binary_tree
   and verify that it passes the unit test.
*)

let number_of_leaves_alt t_init =
  fold_right_binary_tree (fun a -> 1)
    (fun (c1, c2) -> c1 + c2 )
    t_init;;


let () = assert (test_number_of_leaves number_of_leaves_alt);;

(* ********** *)

let test_number_of_nodes candidate =
     (candidate (Leaf 10) = 0)
  && (candidate (Node (Leaf 33,
                       Leaf 44)) = 1)
  && (candidate (Node (Node (Leaf 33,
                             Leaf 34),
                       Leaf 44)) = 2)
  && (candidate (Node (Node (Leaf 33,
                             Node (Leaf 34,
                                   Leaf 35)),
                       Node (Leaf 44,
                             Leaf 45))) = 4)
  (* etc. *);;

let number_of_nodes t_init =
  let rec count t =
    match t with
    | Leaf _ ->
       0
    | Node (t1, t2) ->
       let c1 = count t1
       and c2 = count t2
       in c1 + c2 + 1
  in count t_init;;

let () = assert (test_number_of_nodes number_of_nodes);;

(* Exercise 3:

   Express number_of_nodes using fold_right_binary_tree
   and verify that it passes the unit test.
*)


let number_of_nodes_alt t_init =
  fold_right_binary_tree (fun a -> 0)
    (fun (c1, c2) -> c1 + c2 + 1)
    t_init;;


let () = assert (test_number_of_nodes number_of_nodes_alt);;


(* ********** *)

let test_weight candidate_weight =
 (* test_weight : (binary_tree -> int) -> bool *)
    (candidate_weight (Leaf 10)
     = 10)
  &&
    (candidate_weight (Node (Leaf 1,
                             Leaf 10))
     = 11)
  &&
    (candidate_weight (Node (Node (Leaf 1,
                                   Leaf 10),
                             Node (Leaf 100,
                                   Leaf 1000)))
     = 1111)
  &&
    (candidate_weight (Node (Node (Node (Leaf 1,
                                         Leaf 1),
                                   Leaf 10),
                             Node (Leaf 100,
                                   Leaf 1000)))
     = 1112)
  (* etc. *);;

let weight t_init =
  let rec measure t =
    match t with
    | Leaf n ->
       n
    | Node (t1, t2) ->
       let c1 = measure t1
       and c2 = measure t2
       in c1 + c2
  in measure t_init;;

let () = assert (test_weight weight);;

(* Exercise 4:

   Express weight using fold_right_binary_tree
   and verify that it passes the unit test.
*)


let weight_alt t_init =
  fold_right_binary_tree (fun a -> a)
    (fun (c1, c2) -> c1 + c2)
    t_init;;

  

let () = assert (test_weight weight_alt);;


(* ********** *)

let test_height candidate_height =
 (* test_weight : (binary_tree -> int) -> bool *)
 (candidate_height (Leaf 10)
  = 0)
 &&
 (candidate_height (Node (Leaf 10,
                          Leaf 20))
  = 1)
 &&
 (candidate_height (Node (Leaf 10,
                          Node (Leaf 20,
                                Leaf 30)))
  = 2)
 &&
 (candidate_height (Node (Leaf 10,
                          Node (Leaf 20,
                                Node (Leaf 30,
                                      Leaf 40))))
  = 3)
 &&
 (candidate_height (Node (Node (Leaf 10,
                                Leaf 19),
                          Node (Leaf 20,
                                Node (Leaf 30,
                                      Leaf 40))))
  = 3)
 &&
 (candidate_height (Node (Node (Node (Leaf 10,
                                      Leaf 11),
                                Leaf 19),
                          Node (Leaf 20,
                                Node (Leaf 30,
                                      Leaf 40))))
  = 3)
 &&
 (candidate_height (Node (Node (Node (Leaf 10,
                                      Node (Leaf 11,
                                            Leaf 12)),
                                Leaf 19),
                          Node (Leaf 20,
                                Node (Leaf 30,
                                      Leaf 40))))
  = 4)
 (* etc. *);;

let maximum (i, j) =
  if i < j
  then j
  else i;;
  
let height t_init =
  let rec measure t =
    match t with
    | Leaf n ->
       0
    | Node (t1, t2) ->
       let c1 = measure t1
       and c2 = measure t2
       in maximum (c1, c2) + 1
  in measure t_init;;

let () = assert (test_height height);;

(* Exercise 5:

   Express height using fold_right_binary_tree
   and verify that it passes the unit test.
*)

let height_alt t_init =
  fold_right_binary_tree (fun a -> 0)
    (fun (c1, c2) -> maximum(c1, c2) + 1)
    t_init;;


let () = assert (test_height height_alt);;

(* ********** *)

let test_width_int candidate_width =
 (* test_width_int : (int binary_tree -> int) -> bool *)
    (candidate_width (Leaf 1)
     = 1)
  &&
    (candidate_width (Node (Leaf 1,
                            Leaf 2))
     = 2)
  &&
    (candidate_width (Node (Leaf 1,
                            Node (Leaf 1,
                                  Leaf 2)))
     = 2)
  &&
    (candidate_width (Node (Node (Leaf 1,
                                  Leaf 2),
                            Leaf 2))
     = 2)
  &&
    (candidate_width (Node (Node (Leaf 1,
                                  Leaf 2),
                            Node (Leaf 3,
                                  Leaf 4)))
     = 4)
  &&
    (candidate_width (Node (Node (Node (Leaf 1,
                                        Leaf 2),
                                  Leaf 2),
                            Node (Node (Leaf 3,
                                        Leaf 4),
                                  Node (Leaf 5,
                                        Leaf 6))))
     = 6)
  &&
    (candidate_width (Node (Node (Node (Node (Leaf 1,
                                              Leaf 2),
                                        Leaf 2),
                                  Leaf 2),
                            Node (Node (Leaf 3,
                                        Node (Leaf 3,
                                              Leaf 4)),
                                  Node (Leaf 5,
                                        Leaf 6))))
     = 6)
  (* etc. *);;

(* Exercise 6 (optional because it is harder):

   a. Implement a function that computes the width of a binary tree
      and verify that it passes the unit test.

   b. Express your function using fold_right_binary_tree
      and verify that it passes the unit test.
*)

(*
let width t_init =
  ...

let () = assert (test_width_int width);;
*)

(*
let width_alt t_init =
  ... fold_right_binary_tree ...;;

let () = assert (test_width_int width);;
*)

(* ********** *)

let test_mirror candidate_mirror =
 (* test_mirror : (binary_tree -> binary_tree) -> bool *)
     (candidate_mirror
        (Leaf 10)
      = (Leaf 10))
  && (candidate_mirror
        (Node (Leaf 10,
               Leaf 20))
      = (Node (Leaf 20,
               Leaf 10)))
  && (candidate_mirror
        (Node (Leaf 10,
               Node (Leaf 20,
                     Leaf 30)))
      = (Node (Node (Leaf 30, 
                     Leaf 20),
               Leaf 10)))
  && (candidate_mirror
        (Node (Node (Leaf 10,
                     Leaf 20),
               Node (Leaf 30,
                     Leaf 40)))
      = (Node (Node (Leaf 40,
                     Leaf 30),
               Node (Leaf 20,
                     Leaf 10))))
  (* etc.*);;

let mirror t_init =
  let rec across t =
    match t with
    | Leaf n ->
       Leaf n
    | Node (t1, t2) ->
       let c1 = across t1
       and c2 = across t2
       in Node (c2, c1)
  in across t_init;;

let () = assert (test_mirror mirror);;

(* Exercise 7:

   Express mirror using fold_right_binary_tree
   and verify that it passes the unit test.
*)


let mirror_alt t_init =
  fold_right_binary_tree (fun a -> Leaf a)
    (fun (c1, c2) -> Node (c2, c1))
    t_init;;


let () = assert (test_mirror mirror_alt);;


(* ********** *)

(* end of week-10a_fold-right-for-binary-trees.ml *)

"week-10a_fold-right-for-binary-trees.ml"
