(* TAN YAO HONG BRYAN ASSIGNMENT NO. 11A *)

(* question 1 w/ extra-credit *)

let rec fold_left f b l =
  match l with
  | [] -> b
  | x :: xs -> fold_left f (f b x) xs;;


let rec fold_right f b l =
  match l with
  | [] -> b
  | x :: xs -> f x (fold_right f b xs);;

let array_fold_left f init arr =
  let len = Array.length arr in
  let rec helper j acc =
    if j = len then acc
    else helper (j + 1) (f acc arr.(j))
  in
  helper 0 init;;


let array_fold_right f init arr =
  let len = Array.length arr in
  let rec helper j acc =
    if j = len then acc
    else helper (j + 1) (f arr.(len - j - 1) acc)
  in
  helper 0 init;;
(* question 2 *)

(* how a growable stack works *) 

(* the way the growable array-based stacks work is that the stack is comprised of a reference to an int array and a reference to an int. The int array keeps track of the actual elements in the stack, while the int keeps track of the number of elements in the stack. when the number of elements in the stack is larger than the size of the stack (i.e. when push is called when the stack is full and idx > length of arr) then we create a new array with double the size and initialize it with the elements in the original array and fill the remaining space with arr.(0) *)

(* how we can make it shrinkable *)

(* a way to allow the array to shrink as well is to change pop to something similar to the current push for the growable array-based stacks. we could add a condition to pop to check if idx is lesser than or equal to the half the length of the array (idx <= len/2). if idx <= len/2, then we halve the array to save memory when the stack is small. *)

(* time bound of growable & shrinkable stack *) 

(* the same way amortized analysis works for the push function for growable stacks, it works for shrinkable stacks. the 'pre-pay' moment occurs during the computationally heavier but rare occurance when the stack is full, for push. other than that instance, pushing an element to the stack is constant time. this pre-pay is fully used up by the remaining operations (the number of which is the pre-pay, n) before the next pre-pay, and thus we can assert that push has a O(1) amortized time bound.

similarly, pop, in my implementation, would only have a computationally heavier operation when the condition of idx <= len/2 is met. if not, pop will take O(1). thus, whenever the condition is met and we pre-pay O(n), the number of computations, before the next time we have to pre-pay, this running time is offset by the fact that each computation until then will only take O(1). In general, if there were n + 1 pushes to an array size n, then the n+1th push would take O(n) to double to size of the array. Since there were n+1 operations total, this takes O(1) because all the other operations besides the n+1th push takes O(1). Thus, we can assert that pop has a O(1) amortized time bound in this implementation. *)

(* my implementation *)
(* everything else except pop is similar to prof. hobor's implementation in class_11a *)

type stack = (int array ref) * (int ref);;

exception StackFull;;

let makestack n : stack =
  (ref (Array.make n 0), ref 0);;

let rec push x s =
  match s with
    (rarr, idx) -> let arr = !rarr in
                   let len = Array.length arr in
                   if !idx = len
                   then begin
                      let newarr = Array.init (2 * len)
                                     (fun i -> if i < len then arr.(i) else arr.(0)) in
                      rarr := newarr;
                      push x s
                    end
                  else begin
                      arr.(!idx) <- x;
                      idx := !idx + 1
                    end;;

(* my implementation of pop *)
let rec pop (s : stack) =
  match s with
    (rarr, idx) -> let arr = !rarr in
                   let len = Array.length arr in
                   if !idx = 0 then None
                   else if !idx <= (len / 2) then
                     begin
                       let newarr = Array.init (len / 2)
                                      (fun i -> if i <= !idx then arr.(i) else arr.(0)) in
                       rarr := newarr;
                       pop s
                     end
                   else begin
                       idx := !idx - 1;
                       Some arr.(!idx)
                     end;;
let sstk = makestack 3;;

(* question 3 *)

exception FullQueue;;
exception EmptyQueue;;

type 'a option = Some of 'a | None;;
type 'a queue = 'a option array * int ref * int ref;;

let empty_queue () : 'a queue =
  (*first int ref is for enq, second is for deq *)
  (Array.make 100 None, ref 0, ref 0);;


let rec enqueue (q : 'a queue) e =
  match q with 
    (arr, eqidx, dqidx) -> let len = Array.length arr in
                           if !eqidx = !dqidx - 1 then
                             raise FullQueue
                           else if !eqidx < len then
                             begin
                               arr.(!eqidx) <- e;
                               eqidx := !eqidx + 1
                             end
                           else if !eqidx >= len then
                             if (!dqidx = 0) then
                               raise FullQueue
                             else
                               begin
                                 eqidx := 0;
                                 enqueue q e
                               end;;
let dequeue (q : 'a queue) =
  match q with
    (arr, eqidx, dqidx) -> let len = Array.length arr in
                           if (!dqidx = !eqidx) then
                             raise EmptyQueue
                           else if !dqidx < (len - 1) then
                             begin
                               dqidx := !dqidx + 1;
                               let stor = arr.(!dqidx - 1) in
                               arr.(!dqidx - 1) <- None;
                               stor
                             end
                           else 
                               begin
                                 dqidx := 0;
                                 let stor = arr.(len - 1) in
                                 arr.(len - 1) <- None;
                                 stor
                               end;;
