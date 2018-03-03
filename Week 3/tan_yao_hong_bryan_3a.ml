(* TAN YAO HONG BRYAN ASSIGNMENT NUMBER 3a *)

(*question 1*)

let rec take n l =
  if n = 0 then [] else
    match l with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs;;

let rec drop n l =
  if n = 0 then l else
    match l with
    | [] -> []
    | x :: xs -> drop (n - 1) xs;;

let length l =
  let rec helper l acc = 
    match l with
    | [] -> acc
    | x :: xs -> helper xs (acc + 1)
  in helper l 0;;

let rec f_duplicates x y =
  match x, y with
  | [], [] -> []
  | [], l -> l           
  | l, [] -> l
  | hx :: tx, hy :: ty -> if hx < hy then hx :: f_duplicates tx (hy :: ty)
                          else if hx > hy then hy :: f_duplicates (hx :: tx) ty
                          else hx :: f_duplicates tx ty;;

let rec remove_duplicates l =
  match l with
  | [] -> []
  | [x] -> [x]
  | _ ->
     let len = (length l)/2 in
     let left = take len l in
     let right = drop len l in
     f_duplicates (remove_duplicates left) (remove_duplicates right);;



(*extra credit: 
remove_duplicates functions the same as the function msort from chapter 5 that splits a list into two halves. Splitting a list into two halves until every element of that list is an individual list takes time proportional to the number of times you can split that list, or O(log base 2 n) = O(log n). 

f_duplicates functions almost the same as the function merge from chapter 5. However, it has an additional condition for when hx = hy to remove one of the duplicates. In the worst case scenario, a list L contains all duplicates, and f_duplicates will have to do an additional operation compared to merge every single time. However, f_duplicates has a running time proportional to the length of the list L, and an additional step will simply lead to a running time multiplied by a constant k, which would be dropped when considering O(f_duplicates). Thus, O(f_duplicates) = O(k*n) = O(n). One may also consider that f_duplicates removes the offending duplicate element, leading to less comparisons overall. 

Overall, remove_duplicates calls f_duplicates each time it splits into half. Thus, f_duplicates will run for O(n) every time remove_duplicates splits the list into half, which is O(log n), and thus O(remove_duplicates) = O(n log n) < O(n**2). *)

(*question 2*)


let rec merge x y =
  match x, y with
  | [], l -> l
  | l, [] -> l
  | hx::tx, hy::ty ->
     if hx < hy then hx :: merge tx (hy :: ty)
     else hy :: merge (hx :: tx) ty;;

let rec msort l =
  match l with
  | [] -> []
  | [x] -> [x]
  | _ -> let len = (length l)/2 in
         let left = take(len) l in
         let right = drop(len) l in
         merge (msort left) (msort right);;


let rec select n l =
  let lst = msort l in
  match lst with
  | h :: t -> if n = 0 then h
              else select (n - 1) t;;
                     
                  
(*for select, since the question specifies that we can assume 0<= j < length of list, where j=0 is the smallest and j=n-1 is the largest, I have intentionally left the pattern-matching as not exhaustive to maintain select : int -> 'a list -> 'a. I have also not implemented any limits as to what n can be since the input j is already limited in the specification to 0<=j< length of list *)


(*question 3*)



(*O(group_sort) = O(n**2), where sort = merge sort)



explanation: 


sort (a :: b :: c :: d :: e :: []) will sort the first 5 elements of the list, before concatenating to group_sort tail. group_sort tail will recursively call the list without elements a,b,c,d,e; thus, on every recursive call, the list becomes 5 elements shorter until it has <5 elements in which the remaining elements are sorted.  

Since (group_sort tail) only reduces the list length by a constant (k = 5) and not a factor, the running time is proportional to n and will be roughly n/5. Hence, O(group_sort tail) = O(n/5) = O(n).
Since I am using merge sort as my sort of choice, O(msort) = O(nlogn). However, sort (a :: b :: c :: d :: e :: []) will run on a constant number of elements every time it is called. Thus, O(sort (a :: b :: c :: d :: e :: [])) = O(5 * log 5) = O(k) = O(1). 

(group_sort tail) and sort(a :: b :: c :: d :: e :: []) are also concatenated every time group_sort is called. Concatenation (list z @ list x) has O(z). Initially, the concatenation would be constant time, considering that sort(a :: b :: c :: d :: e :: []) is the first list and it has a constant number of elements. However, as (group_sort tail) recursively iterates, the number of pending concatenations increases. After all the recursive calls are complete, the concatenation is carried out from left to right in order of pending concatenations. This means that the time taken for the concatenation grows by a constant k = 5 every time a concatenation is carried out. As a result, the running time for the concatenation increases by 5 every time it is carried out, and the running time increases linearly with the final concatenation taking (n - 5). Thus, O(concatenation) = O(n) as n is the leading variable.

Thus, since every time group_sort is called sort (a :: b :: c :: d :: e :: []) is called and concatenated to (group_sort tail), the total work done is roughly some constant k  *  n (from concatenation) * n (from group_sort tail) * 1 (from sort (a::b::c::d::e::[])) = k*n**2. Therefore, O(group_sort) = O(n**2) *) 


