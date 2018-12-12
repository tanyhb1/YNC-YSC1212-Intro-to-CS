(* TAN YAO HONG BRYAN ASSIGNMENT no. 8b *)

(* question 1 *)

type pn_token = Num of int | Plus | Minus | Times;;
type pn_string = pn_token list;;

let (pn_empty : pn_string) = [];;
exception Bad;;
          
let rev l =
  let rec rev l acc =
    match l with
    | [] -> acc
    | h :: t -> rev t (h :: acc)
  in rev l [];;

let simple_eval operator top below =
  match operator with
  | Plus -> (match top, below with
             | (Num a), (Num b) -> a + b
             | _, _ -> raise Bad)        
  | Minus ->(match top, below with
             | (Num a), (Num b) -> a - b
             | _, _ -> raise Bad)
  | Times -> (match top, below with
              | (Num a), (Num b) -> a * b
              | _, _ -> raise Bad)
  | _ -> raise Bad;;


let eval (expr : pn_string) =
  let rec eval_helper expr (stk : pn_string) =
    match expr with
    | h :: t -> (match h with
                 | Num a -> eval_helper t (h :: stk)
                 | Plus | Minus | Times ->
                    (match stk with
                     | (x :: xs) -> (match xs with
                                     | [] -> raise Bad
                                     | x' :: xs' -> eval_helper t (Num (simple_eval h x x') :: xs'))
                     | [] -> raise Bad))
    | [] -> match stk with
            | h :: [] -> (match h with
                          | Num a -> a
                          | _ -> raise Bad)
            | _ -> raise Not_found
    in eval_helper (rev expr) pn_empty;;

let exm = [Plus; Num 5; Plus; Times; Num 2; Num 7; Num 9];;
let test1 = [Times; Minus; Num 7; Num 12; Times; Plus; Num 5; Num 8; Minus; Num 10; Num 16];;
let test2 = [Times; Plus; Num 3; Num 4; Times; Plus; Num 5; Num 6; Num 7];;

(* question 2 *)
(* i apologize in advance for the large number of helper functions, I have tried to demarcate where save_tree and load_tree are using comments *)

type 'a tree =
  | Lf
  | Br of 'a * 'a tree * 'a tree
  | Num of int;;
type 'a tree_list = 'a tree list;;
type pn_tree =
  | Num of int
  | L
  | B;;
type pn_tree_string = pn_tree list;;

                      
let a_tree = Br (4, Br(3, Lf, Lf), Br(5, Lf, Lf));;
let b_tree = Br (3, Lf, Br (5, Br(4, Lf, Lf), Lf));;

let rec transform tree =
  match tree with
  | Br (v, Lf, Lf) -> [B; Num v; L; L]
  | Br (v, l, Lf) -> [B; Num v] @ (transform l) @ [L]
  | Br (v, Lf, r) -> [B; Num v; L] @ (transform r)
  | Br (v, l, r) -> [B; Num v] @ ((transform l) @ (transform r))
  | Lf -> [L]
  | _ -> raise Bad;;


let rec stringify lst =
  match lst with
  | [] -> []
  | h :: t -> (match h with
              | B -> "B"
              | L -> "L"
              | Num a -> (string_of_int a))
              :: stringify t;;
                             
let rec dictionary_to_channel ch d =
  match d with
  | [] -> ()
  | h :: t -> output_string ch h;
              output_string ch "; ";
              dictionary_to_channel ch t;;

(* actual save_tree function is here *)
let save_tree filename tree =
  let ch = open_out filename in
  let tree = stringify (transform tree) in
  let rec tree_to_channel ch tree =
    match tree with
    | [] -> ()
    | h :: t -> output_string ch h;
                output_string ch "\n";
                tree_to_channel ch t
  in tree_to_channel ch tree;
     close_out ch;;


let rec string_of_ch ch =
  try
    let e = input_line ch in
    e :: string_of_ch ch
  with
    End_of_file -> [];;


let rec interpret str =
  match str with
  | [] -> []
  | h :: t -> if h = "L" then L :: interpret t
              else if h = "B" then B :: interpret t
              else (Num (int_of_string h)) :: interpret t;;


let rec reconstruct_tree tree_list stk =
  match tree_list with
  | [] -> (match stk with
           | [x] -> x
           | _ -> raise Bad)
  | h :: t -> (match h with
               | L -> reconstruct_tree t (Lf :: stk)
               | Num a -> reconstruct_tree t ((Num a) :: stk)
               | B -> (match stk with
                       | x :: xs ->
                          (match xs with
                           | x2 :: xs2 ->
                              (match xs2 with
                               | x3 :: xs3 ->
                                  (match x with
                                   | Num a -> reconstruct_tree t (Br(a,x2,x3) :: xs3)
                                   | _ -> raise Bad)
                               | _ -> raise Bad)
                           | _ -> raise Bad)
                       | _ -> raise Bad));;

(* actual load_tree function is here *)
let load_tree filename =
  let ch = open_in filename in
  let str = string_of_ch ch in
  close_in ch;
  reconstruct_tree (rev (interpret str)) [];;

                     
