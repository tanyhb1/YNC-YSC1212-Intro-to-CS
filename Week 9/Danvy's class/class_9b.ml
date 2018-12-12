(* class 9b *)

let test_last candidate_last =
  (candidate_last [] = None) &&
    (candidate_last [10] = Some 10) &&
      (candidate_last [10; 20] = Some 20) &&
        (candidate_last [10; 20; 30] = Some 30);;

let last xs =
  match xs with
  | [] -> None
  | x :: xs ->
     Some (let rec traverse candidate_last_element xs =
             match xs with
             | [] ->
                candidate_last_element
             | x :: xs' ->
                traverse x xs'
           in traverse x xs);;

let () = assert (test_last last);;

let fold_right_list nil_case cons_case xs_init =
  let rec visit xs =
    match xs with
    | [] ->
       nil_case
    | x :: xs' ->
       cons_case x (visit xs')
  in visit xs_init;;

let last' xs =
  match xs with
  | [] -> None
  | x :: xs ->
     Some (let rec traverse xs candidate_last_element =
             match xs with
             | [] -> candidate_last_element
             | x :: xs' ->
                traverse xs' x
           in traverse xs x);;



let last'' xs =
  match xs with
  | [] -> None
  | x :: xs ->
     Some ((let rec traverse xs =
             match xs with
             | [] ->
                (fun candidate_last_element -> candidate_last_element)
             | x :: xs' ->
                (fun candidate_last_element ->  (traverse xs') x)
           in traverse xs) x);;


let last_alt xs =
  match xs with
  | [] -> None
  | x :: xs ->
     Some ((fold_right_list (fun candidate_last_element -> candidate_last_element)
             (fun x c -> (fun candidate_last_element -> c x))
             xs)
            x);;
