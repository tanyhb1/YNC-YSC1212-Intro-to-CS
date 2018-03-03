print_string "Hello, world!\n";;
(* globally defined x*)

let x = 10;;
let add2x k = k+x;;
add2x 10;;

(*locally defined x will not affect functions defined prior*)
let x = 12;;
add2x 10;;

(*locally defined x in a function*)
let x = 15 in x + x;;

(*recursion*)
let rec fact q =
  if q = 0 then 1
  (*or, if q = 1 then 1*)
           else q * fact(q-1);;
fact 0;;

(*exponentiation*)
let rec exp a b =
  if b = 0 then 1
  else a * (exp a (b-1));;

exp 10 2

let isodd x =
  x mod 2 = 1;;


let rec fast_exp a b =
  if b = 0 then 1
  else if b = 1 then a 
  else let halfway = fast_exp a (b / 2)in
       if isodd b then a * halfway * halfway
       else halfway * halfway;;
let rec fast_exp' a b =
  if b = 0 then 1
  else if b = 1 then a 
  else let halfway = fast_exp a (b / 2)in
      (if isodd b then a else 1) * halfway * halfway;;
                        
fast_exp 2 32;;
fast_exp 2 9;;

(*chapter 1 *)

1 <> 1;;
true || false;;
true && false;;
if true then false else true;;
'a'+'b';;

1 + 2 mod 3;;
(1 + 2) mod 3;;
1 + (2 mod 3);;

min_int;;
max_int;;
max_int + 1;;
min_int - 1;;

1/0;;

5 mod 4;;
5 mod -4;;
0 mod 4;;
4 mod 0;;

'a' < 'b';;
'b' < 'a';;
'A' < 'a';;
'z' < 'a';;
'a';;
'Z' < 'a';;
'a' = true;;

let cube x = x * x * x;;
let b = 500 in cube b;;

(* chapter 2 *)
let x10 no = no * 10;;

let berl a b =
  if a <> 0 && b <> 0 then true
  else false;;
berl 1 2;;
berl 1 0;;
berl 0 0;;

let rec adder x =
  if x = 1 then 1
  else x+adder(x-1);;
adder 5;;

let rec power x n =
  if n = 0 then x
  else x * power x (n-1);;

power 5 5;;

let isvowel x =
  if x = 'a' || x = 'e' || x = 'i' || x =  'o' || x = 'u' then true
  else false;;

isvowel 'a';;
isvowel 'z';;

let isconsonant c =
  if isvowel c = true then false else true;;

isconsonant 'c';;
isconsonant 'e';;

let x = 1 in let x = 2 in x + x;;

let rec fact a =
  if a = 1 then 1
  else if a > 0 then a * fact (a - 1)
  else if a < 0 then a * fact (a + 1)
  else 0;;

fact 5;;
fact 0;;
fact -5;;
