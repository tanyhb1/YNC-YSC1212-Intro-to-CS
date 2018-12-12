(* danvy's class *)
(* on extrapolating standard recursive cases as fold_right,
   and tail-recursive cases as fold_left,
   and how to write programs efficiently using these programming paradigms *)


type 'a polymorphic_list = Nil | Cons of 'a * 'a polymorphic_list;;

let test_length_int candidate =
  (candidate [] = 0) &&
    (candidate [1] = 1 ) &&
      (candidate [2; 1] = 2) &&
        (candidate [3; 4; 2; 1] = 3);;

let length vs_init =
  let rec visit vs =
    match vs with
    | [] -> 0
    | x :: xs ->
       let c = visit xs
       in 1 + c
  in visit vs_init;;



let () = assert (test_length_int length);;

let fold_right_list nil_case cons_case xs_init =
  let rec visit xs =
    match xs with
    | [] ->
       nil_case
    | x :: xs' ->
       cons_case x (visit xs')
  in visit xs_init;;

let test_copy candidate =
  (candidate [] = []) &&
    (candidate [1;2;3] = [1;2;3]) &&
      (candidate [1] = [1]);;



let copy vs_init =
  let rec visit vs =
    match vs with
    | [] ->
       []
    | v :: vs' ->
       let c = visit vs'
       in v :: c
  in visit vs_init;;

let () = assert (test_copy copy);;


let test_show_list_int candidate =
  (candidate [] = "[]" ) &&
    (candidate [1] = "1 :: []") &&
      (candidate [2; 1] = "2 :: 1 :: []");;

let show_int n =
  string_of_int n;;

let show_bool b =
  if b
  then "true"
  else "false";;


let show_list show_yourself vs_init =
  let rec visit vs =
    match vs with
    | [] -> "[]"
    | v :: vs' -> let c = visit vs'
                  in show_yourself v ^ " :: " ^ c
  in visit vs_init;;

let () = assert (test_show_list_int (show_list show_int));;

let show_list_alt show_yourself vs_init
  = fold_right_list "[]" (fun v c -> show_yourself v ^ " :: " ^ c) vs_init;;

let () = assert (test_show_list_int (show_list_alt show_int));;

(* how to express fold_left (tail-recursion) in fold_right (standard recursion) *)
