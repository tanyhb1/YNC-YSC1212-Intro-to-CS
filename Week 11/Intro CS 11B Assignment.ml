(* File needed for Assignment 11B 
   Aquinas Hobor
*)

let rec fold_left f i l =
  match l with
    | [] -> i
    | h :: t -> fold_left f (f i h) t;;

let input_catalog () = 
  let ic = open_in "Museum.cat" in
  let rec helper acc =
    try
      let name = input_line ic in
      let weight = float_of_string (input_line ic) in
      let value = float_of_string (input_line ic) in
        helper ((name, weight, value) :: acc)
    with 
        _ -> acc 
  in 
  let choices = helper [] in
    close_in ic;
    choices;;

let total_weight lst =
  fold_left (fun acc (_, w, _) -> acc +. w) 0. lst;;

let total_value lst =
  fold_left (fun acc (_, _, v) -> acc +. v) 0. lst;;

(* Your task: fill in the ... *)
let rob items capacity = ...;;

let items = input_catalog ();; (* Load in Museum.cat *)
let plan = rob items 18.68;; (* We can carry 18.68 kg *)

total_value plan;; (* Should be 3431.21122 *)

(* I will test your file with a moderately different Museum.cat,
   just to make sure everything works well ;-)
*)
