(*Class 11a/Floats & Array-based stacks/queues *)

3.14;;
7.;;
(* have to use floating point addition; very clear demarcation between float and int *)
7 + 2.3;;
7.6 +. 16.4;;
(* round-off as a result of hardware independent of programming languages *)
1.11 +. 2.03;;
1.11 +. 2.03 = 3.14;;

max_float;;
min_float;;


(* comparison allowing for a margin of error in calculation due to round-off *)
let abs_float a =
  if a < 0.0 then 0. -. a
  else a;;

let eq_float a b =
  let epsilon = 0.0000000000001 in
  if abs_float (a -. b) < epsilon then true else false;;


(* division *)

3.0 /. 0.01;;
3.0 /. 0.0001;;
(* infinity clap *)
3.0 /. 0.0;;
infinity +. 4.2;;
infinity -. infinity;;

(* Array-based stacks & queues *)

type stack = (int array) * (int ref);;

exception StackFull;;

let makestack n : stack =
  (Array.make 0 n);;

(* stacks *)

type 'a stack = ('a array) * (int ref);;
let makestack n : stack =
  (Array.make 0 n, ref 0);;

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


(* Problem 1 : How can we remove the stack full problem *)

let arr = [|1; 2; 3; 4; 5|];;
let arr2 = Array.init (2 * Array.length arr)
             (fun i -> if i < Array.length arr then arr.(i) else arr.(0));;

let push x s =
  match s with
  | (arr, index) -> let len = Array.length arr in
                    if !index = Array.length arr
                    then begin
                        let newarr = Array.init (2 * len)
                                       (fun i -> if i < len then arr.(i) else arr.(0))
                        in
                        push x (newarr, index)
                        end
                    else begin
                        arr.(!index) <- x;
                        index := !index + 1
                      end
;;

let makestack n =
  ref((Array.make 0 n, ref 0));;



let rec push x s =
  match s with
  | (rarr, index) ->
     let arr = !rarr in
     let len = Array.length arr in
     if !index = Array.length arr
     then begin
         let newarr = Array.init (2 * len)
                        (fun i -> if i < len then arr.(i) else arr.(0))
         in
         rarr := newarr;
         push x s
       end
     else begin
         arr.(!index) <- x;
         index := !index + 1
       end
;;

(* problem 2: what is the time bound for push and pop now? *)

(* problem 3: double-ended queues (list based) *)

type 'a dequeue = ('a list) * ('a list);;

let mt_dq = ([], []);;

let cons (x : 'a) (dq : 'a dequeue) =
  match dq with (front, back) -> (x :: front, back);;

let snoc x dq =
  match dq with (front, back) -> (front, x :: back);;

let rec takedrop n l =
  if n = 0 then ([], l) else
    match l with
    | [] -> ([], [])
    | h :: t ->
;;


(* to make array-based queue, create two indices f and b that point to the front and the back respectively, and augment them when push and pop respectively *)
(* take note of full queue, what to do then? unreasonable to create new queue, since the front might be empty - wrap the b indices back to the front? *)
(* challenge in wrapping around properly and determining when to fuse a new queue into the current queue for space *)
