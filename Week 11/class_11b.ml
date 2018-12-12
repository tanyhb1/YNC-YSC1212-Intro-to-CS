(* Class 11b *)

type qnode = NilQ
           | NodeQ of int * qnode ref * qnode ref;;
type q = (qnode ref) * (qnode ref);;

(* chapter 15 *)

List.length [1;2;3;4;5];;
List.nth [1;2;4;8;16] 3;;

(*question 1*)

List.concat [1;2;3;4;5];;
List.concat;;

List.concat [[1;2;3]; [4;5;6]; [7;8;9]];;
let l1 = [[1;2;3]; [4;5;6]; [7;8;9]];;
          

let rec concat l =
  match l with
  | [] -> []
  | h :: t -> h @ concat t;;
let rev l =
  let rec helper l acc =
  match l with
  | [] -> acc
  | h :: t -> rev t (h :: acc)
  in helper l [];;


let concat_t l =
  let rec helper l acc =
  match l with
  | [] -> rev acc
  | h :: t -> helper t (rev h @ acc)
  in helper l [];;

(*question 2*)
List.mem;;
List.mem 0 [1;2;3;4];;

let rec true_checker (l: bool list list) =
  match l with
  | [] -> false
  | h :: t -> if List.mem true h then true else true_checker t;;

(*question 3*)

let exc_counter (s: string) =
  let n = ref 0 in
  begin
  String.iter (function '!' -> n := !n + 1 | _ -> ()) s;
  !n;
  end;;

    
(*quesiton 4 *)
let period =
  String.map (fun x -> match x with
                       |'!' -> '.'
                       | x -> x);;
(*quesiton 5*)

let str_concat =
  String.concat "";;

(*question 6*)

let concat s =
  let b = Buffer.create 100 in
  List.iter (Buffer.add_string b) s;
  Buffer.contents b;;

(*question 7*)

let occurances c s =
  if s = "" then 0 else
    let num = ref 0 in
    let str = ref s in
    while
      String.length s <= String.length !str && !str <> ""
    do
      if String.sub !str 0 (String.length c) = c then
        num := !num + 1;
      str := String.sub !str 1 (String.length !str - 1)
    done;
    !num
;;
