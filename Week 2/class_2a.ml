        OCaml version 4.06.0

# let rec rev l = match l with
  | [] -> []
  | x :: xs -> rev (xs @ [x]);;
    val rev : 'a list -> 'b list = <fun>
# rev [5;3;1];;
  C-c C-cInterrupted.
# let rec count_true l = match l with
  | [] -> 0
  | x :: xs -> if x = true then 1 + count_true xs else count_true xs;;
    val count_true : bool list -> int = <fun>
# count_true [true;false];;
- : int = 1
# let rec rev l = match l with
  | [a] -> [a]
  | x :: xs -> rev (xs @ [x]);;
    Characters 16-73:
  ................match l with
    | [a] -> [a]
    | x :: xs -> rev (xs @ [x])..
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]
val rev : 'a list -> 'a list = <fun>
# rev [5;3;1];
				   ;
				  ;;
    Characters 20-21:
  				   ;
         ^
Error: Syntax error
# rev [5;3;1];;
  C-c C-cInterrupted.
# let rec rev l = match l with
  | [a] -> [a]
  | x :: xs -> rev xs @ [x];;
    Characters 16-71:
  ................match l with
    | [a] -> [a]
    | x :: xs -> rev xs @ [x]..
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]
val rev : 'a list -> 'a list = <fun>
# let rec rev l = match l with
  | [] -> []
  | x :: xs -> rev xs @ [x];;
    val rev : 'a list -> 'a list = <fun>
# rev [5;3;1];;
- : int list = [1; 3; 5]
# let rec rev l = match l with
  | [] -> []
  | x :: xs -> rev xs :: [x];;
    Characters 57-63:
    | x :: xs -> rev xs :: [x];;
                 ^^^^^^
Error: This expression has type 'a list
       but an expression was expected of type 'a
       The type variable 'a occurs inside 'a list
# let rec rev l = match l with
  | [] -> []
  | x :: xs -> rev xs @ [x];;
    val rev : 'a list -> 'a list = <fun>
# let rec palindrome l =
  rev l;;
  val palindrome : 'a list -> 'a list = <fun>
# rec palindrome 'hello';;
Characters 0-3:
  rec palindrome 'hello';;
  ^^^
Error: Syntax error
# palindrome 'hello'
;;
  Characters 11-12:
  palindrome 'hello'
             ^
Error: Syntax error
# palindrome ['hello'];;
Characters 12-13:
  palindrome ['hello'];;
              ^
Error: Syntax error
# let rec palindrome l =
  l @ rev l;;
  val palindrome : 'a list -> 'a list = <fun>
# palindrome [5;3;2];;
- : int list = [5; 3; 2; 2; 3; 5]
# let rec drop_last l =
  | [a] -> []
  | x :: xs -> x :: drop_last xs;;
    Characters 24-25:
    | [a] -> []
    ^
Error: Syntax error
# let rec drop_last l = match l with
  | [a] -> []
  | x :: xs -> x :: drop_last xs;;
    Characters 22-81:
  ......................match l with
    | [a] -> []
    | x :: xs -> x :: drop_last xs..
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]
val drop_last : 'a list -> 'a list = <fun>
# drop_last [5;3;1;2];;
- : int list = [5; 3; 1]
# let member a l = match l with
  | [] -> false 
  | x : xs -> if x <> a then false else member a xs;;
    Characters 53-54:
    | x : xs -> if x <> a then false else member a xs;;
        ^
Error: Syntax error
# let member a l = match l with
  | [] -> false 
  | x :: xs -> if x <> a then false else member a xs;;
    Characters 88-94:
    | x :: xs -> if x <> a then false else member a xs;;
                                           ^^^^^^
Error: Unbound value member
# let rec member a l = match l with
  | [] -> false 
  | x :: xs -> if x <> a then false else member a xs;;
    val member : 'a -> 'a list -> bool = <fun>
# member 1 [4;3;2;5;1;2;3;4;5;1];;
- : bool = false
# let rec member a l = match l with
  | [] -> false 
  | x :: xs -> if x = a then false else member a xs;;
    val member : 'a -> 'a list -> bool = <fun>
# member 1 [1;2;3;4]
  ;;
  - : bool = false
# let rec member a l = match l with
  | [] -> false 
  | x :: xs -> if x = a then true else member a xs;;
    val member : 'a -> 'a list -> bool = <fun>
# member 1 [1;2;3;4];;
- : bool = true
# let rec make_set l = match l with
  | [] ->
  | x :: xs -> if member x xs = true then x :: make_set xs else make_set xs;;
    Characters 46-47:
    | x :: xs -> if member x xs = true then x :: make_set xs else make_set xs;;
    ^
Error: Syntax error
# let rec make_set l = match l with
  | [] -> []
  | x :: xs -> if member x xs = true then x :: make_set xs else make_set xs;;
    val make_set : 'a list -> 'a list = <fun>
# make_est [5;3;2;1;5;2;4];;
Characters 0-8:
  make_est [5;3;2;1;5;2;4];;
  ^^^^^^^^
Error: Unbound value make_est
Hint: Did you mean make_set?
# make_set [5;3;2;1;5;2;4];;
- : int list = [5; 2]
# let rec make_set l = match l with
  | [] -> []
  | x :: xs -> if member x xs = false then x :: make_set xs else make_set xs;;
    val make_set : 'a list -> 'a list = <fun>
# make_set [5;3;2;1;5;2;4;];;
- : int list = [3; 1; 5; 2; 4]
# let rec rev1 l acc = match l with
  | [] -> acc
  | x :: xs -> rev xs (x @ acc);;
    Characters 63-66:
    | x :: xs -> rev xs (x @ acc);;
                 ^^^
Error: This function has type 'a list -> 'a list
       It is applied to too many arguments; maybe you forgot a `;'.
# let rec rev1 l acc = match l with
  | [] -> acc
  | x :: xs -> rev xs (x @ acc);;
    Characters 63-66:
    | x :: xs -> rev xs (x @ acc);;
                 ^^^
Error: This function has type 'a list -> 'a list
       It is applied to too many arguments; maybe you forgot a `;'.
# let rec rev1 l acc = match l with
  | [] -> acc
  | x :: xs -> rev1 xs (x @ acc);;
    val rev1 : 'a list list -> 'a list -> 'a list = <fun>
# rev1 [5;3;2;1];;
Characters 6-7:
  rev1 [5;3;2;1];;
        ^
Error: This expression has type int but an expression was expected of type
         'a list
# let rec rev1 l acc = match l with
  | [] -> acc
  | x :: xs -> rev1 xs (x @ acc)
    in rev1 l [];;
      Characters 93-94:
      in rev1 l [];;
              ^
Error: Unbound value l
# let rec rev1 l =
  let rec helper l acc = match l with
  | [] -> acc
  | x :: xs -> rev1 xs (x @ acc)
    in helper l [];;
        Characters 109-120:
      in helper l [];;
         ^^^^^^^^^^^
Error: This expression has type 'a list
       but an expression was expected of type 'a list -> 'a list
# let rec rev1 l =
  let rec help_rev l acc = match l with
  | [] -> acc
  | x :: xs -> rev1 xs (x @ acc)
    in help_rev l [];;
        Characters 111-124:
      in help_rev l [];;
         ^^^^^^^^^^^^^
Error: This expression has type 'a list
       but an expression was expected of type 'a list -> 'a list
# let rec rev1 l =
  let rec help_rev l acc = match l with
  | [] -> acc
  | x :: xs -> help_rev xs (x @ acc)
    in help_rev l [];;
        val rev1 : 'a list list -> 'a list = <fun>
# rev1 [5;3;2;1];;
Characters 6-7:
  rev1 [5;3;2;1];;
        ^
Error: This expression has type int but an expression was expected of type
         'a list
# [5;3;2;1];;
- : int list = [5; 3; 2; 1]
# let rec rev1 l =
  let rec help_rev l acc = match l with
  | [] -> acc
  | x :: xs -> help_rev xs (x @ acc)re
    in help_rev l [];;
        val rev1 : 'a list list -> 'a list = <fun>
# rev1;;
- : 'a list list -> 'a list = <fun>
# rev1 [5; 4; 3; 2; 1];;
Characters 6-7:
  rev1 [5; 4; 3; 2; 1];;
        ^
Error: This expression has type int but an expression was expected of type
         'a list
# rev [5;3;2;1];;
- : int list = [1; 2; 3; 5]
# let rec rev1 l =
  let rec help_rev l acc = match l with
  | [] -> acc
  | x :: xs -> help_rev xs (x :: acc)
    in help_rev l [];;
        val rev1 : 'a list -> 'a list = <fun>
# rev1 [5;3;2;1];;
- : int list = [1; 2; 3; 5]
# let rec factorial a = match a with
  | 1 -> 1
  | _ -> a * factorial (a - 1);;
    val factorial : int -> int = <fun>
# factorial 5;;
- : int = 120
# let rec isvowel x = match x with
  | 'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false;;
    val isvowel : char -> bool = <fun>
# isvowel 'hello';;
Characters 8-9:
  isvowel 'hello';;
          ^
Error: Syntax error
# let rec gcd a b = match b with
  | a -> b
  | _ -> gcd b (a mod b);;
    Characters 46-47:
    | _ -> gcd b (a mod b);;
      ^
Warning 11: this match case is unused.
val gcd : int -> int -> int = <fun>
# let rec summer n = match n with
  | 1 -> 1
  | _ -> n + summer (n - 1);;
    val summer : int -> int = <fun>
# summer 5;;
- : int = 15
# summer 3;;
- : int = 6
# let rec expo x n = match n with
  | 1 -> x
  | _ -> x * expo x (n - 1);;
    val expo : int -> int -> int = <fun>
# expo 5 2;;
- : int = 25
# expo 5 3;;
- : int = 125
# 'a'..'z'
;;
  Characters 3-5:
  'a'..'z'
     ^^
Error: Syntax error
# let isupper c = match c with
  | 'A'..'Z' = true
  | _ = false;;
    Characters 42-43:
    | 'A'..'Z' = true
               ^
Error: Syntax error
# let isupper c = match c with
  | 'A'..'Z' -> true
  | _ -> false;;
    val isupper : char -> bool = <fun>
# isupper 'b'
  ;;
  - : bool = false
# isupper 'B';;
- : bool = true
# 