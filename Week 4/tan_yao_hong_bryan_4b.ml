(* TAN YAO HONG BRYAN ASSIGNMENT NO. 4b *)

(* question 1 *)

let rec fold_right f b l =
  match l with
  | [] -> b
  | x :: xs -> f x (fold_right f b xs);;

let filter f l = fold_right (fun a b -> if f a then a :: b else b) [] l;;

(* question 2 *)

let compose_list func_lst = (fun x -> fold_right (fun a b -> a b) x func_lst);;



                             
                              
