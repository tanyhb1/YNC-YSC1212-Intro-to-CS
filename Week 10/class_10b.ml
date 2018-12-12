(* class 10b | further exploration of mutable structures *)
let rec fold_left f b l =
  match l with
  | [] -> b
  | h :: t -> fold_left f (f b h) t;;
let iter_left f = fold_left (fun () i -> f i) ();;


(*array-based map and fold *)

(*initializing an array based on a function *)
Array.init;;

Array.init 10 (fun x -> x);;

(*array map*)
let arr = Array.init 10 (fun x -> x + 10);;

let array_map f arr =
  Array.init(Array.length arr) (fun i -> f arr.(i));;

let arr2 = array_map (fun x -> x mod 2 = 0) arr;;

(*array fold left*)
let array_fold_left f init arr =
  let len = Array.length arr in
  let rec helper j acc =
    if j = len then acc
    else helper (j + 1) (f acc arr.(j))
  in
  helper 0 init;;

let sum = array_fold_left (fun acc x -> x + acc) 0 arr;;

(*array fold left index-based *)

(*binary search in array O(logn) *)
let search (arr : int array) (key : int) =
  let rec helper lower upper =
    if lower = upper then false
    else let midpoint = (upper + lower) / 2 in
         let midvalue = arr.(midpoint) in
         if midvalue = key then true
         else if key < midvalue then helper lower midpoint
         else helper (midpoint + 1) upper
  in
  helper 0 (Array.length arr);;

let arr3 = [|1;2;3;4;5;6|];;

exception StackFull;;

                
(* counting sort*)
(* sorts faster than O *)

(* stacks *)

type 'a stack = ('a array) * (int ref);;
let makestack n : stack =
  (Array.make 0 n, !

(* push = putting new value in and incrementing the int ref *)

let push x s =
  match s with
  | (arr, index) -> if !index = Array.length arr
                    then raise StackFull
                    else begin
                        arr.(!index) <- x;
                        index := !index + 1
                      end
;;

                      
(* pop = subtracting 1 from int ref and taking the value pointed to out *)
type 'a option =  None | Some of 'a;;

let pop s =
  match s with
  | (arr, index) -> if !index = 0 then None
                    else begin
                        index := !index - 1;
                        Some(arr.(!index))
                      end
;;
