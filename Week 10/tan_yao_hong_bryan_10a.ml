(*TAN YAO HONG BRYAN ASSIGNMENT no. 10a *)

(* question 1 *)

(* function for looking up a key in a list of pairs *)
let rec lookup key lst =
  match lst with
  | x :: xs -> (match x with
                | (a, b) -> if a = key then b else lookup key xs)
  | _ -> raise Not_found;;


let cache f =
  (* create ref *)
  let stor = ref [] in
  (fun x ->
    try
      (* try to lookup the key x inside !stor *)
      lookup x !stor
    with
      (* if not found/error raised, then create new entry *)
      _ ->
      (* calculate f x once and store it so no need to calculate it twice *)
      let tmp = f x in
      (* append to list *)
      stor := (x, tmp) :: !stor;
      (* return tmp *)
      tmp);;




(* question 2 *)
type expr =
| Num of int
| Add of expr * expr
| Sub of expr * expr
| Mult of expr * expr
| Div of expr * expr
| And of expr * expr
| Or of expr * expr
| Not of expr
| IfThenElse of expr * expr * expr;;


let rec evaluate e = match e with
  | Num x -> x
  | Add (e1, e2) -> evaluate e1 + evaluate e2
  | Sub (e1, e2) -> evaluate e1 - evaluate e2
  | Mult (e1, e2) -> evaluate e1 * evaluate e2
  | Div (e1, e2) -> evaluate e1 / evaluate e2
  | And (e1, e2) ->
     (* if true && true then 1 else false *)
     if evaluate e1 <> 0 && evaluate e2 <> 0
     then 1
     else 0
  | Or (e1, e2) ->
     (* if either e1 or e2 true then 1 else false *)
     if evaluate e1 <> 0 || evaluate e2 <> 0
     then 1
     else 0
  | Not e ->
     (* if e is true then false else true *)
     if evaluate e <> 0
     then 0
     else 1
  | IfThenElse (e1, e2, e3) ->
     (* if e1 is true then evaluate e2 else evaluate e3 *)
     if evaluate e1 <> 0
     then evaluate e2
     else evaluate e3;;

