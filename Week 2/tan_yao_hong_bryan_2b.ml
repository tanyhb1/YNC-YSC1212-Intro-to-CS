(* TAN YAO HONG BRYAN ASSIGNMENT NUMBER 2B *)

(* question 1 *)

let find_smallest_multiple n =
  let rec helper n acc = match n with
    | 1 -> acc
    | x ->  if acc < 0 then 0 else
              let rec gcd a b = match b with
              | 0 -> a
              | x ->  gcd b (a mod b)
            in helper (n - 1) ((acc * (n - 1)) /(gcd acc (n - 1)))
  in helper n n;;

(* question 2 *)

let factorial_zeros n =
  let rec helper n acc =
    if n = 0 || n = 1 then acc else helper (n/5) (acc + (n/5))
  in helper n 0;;

                              
(* question 3 *)

let minmax l = match l with
  | x :: (xs :: xss) -> if xs > x then helper l x xs else helper l xs x
  (* to make it int list...*)
  | x :: [] when x = 0 -> [0; 0]
  | x :: [] -> [x;x]
  | [] -> [];;

        
let rec helper l min max = match l with
  | x :: (xs :: xss) -> if x > xs then (if x > max && xs < min then helper xss xs x
                                             else if x > max then helper xss min x
                                             else if xs < min then helper xss xs max
                                             else helper xss min max)
                        else (if xs > max && x < min then helper xss x xs
                              else if xs > max then helper xss min xs
                              else if x < min then helper xss x max
                              else helper xss min max)
  | x :: [] -> if x > max then (min :: x :: [])
               else if x < min then (x :: max :: [])
               else (min :: max :: [])
  | [] -> (min :: max :: [])
