(* TAN YAO HONG BRYAN ASSIGNMENT NO. 6a *)

(* question 1 *)

let rec reverse_lookup x l=
  match l with
  | [] -> raise Not_found
  | (name, phone_no) :: t -> if phone_no = x then name else reverse_lookup x t;;

(* question 2 *)

let rev l =
  let rec rev_helper l stor =
    match l with
    | [] -> stor
    | h :: t -> rev_helper t (h::stor)
  in rev_helper l [];;


let partition f l =
  let rec partition_helper lst1 lst2 l=
    match l with
    | [] -> (rev lst1, rev lst2)
    | h :: t -> if f h
                then partition_helper (h::lst1) lst2 t
                else partition_helper lst1 (h::lst2) t
  in partition_helper [] [] l;;
