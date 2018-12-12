(* chapter 12 I/O *)

print_int 100;;

let print_dict_entry (k, v) =
  print_int k;
  print_newline ();
  print_string v;
  print_newline ();;

let rec print_dict d =
  match d with
  | [] -> ()
  | h :: t -> print_dict_entry h;
            print_dict t;;

let rec iter f l =
  match l with
  | [] -> ()
  | h :: t -> f h;
              iter f t;;

(* print_dict defined using iter *)

let rec print_dict' d =
  iter print_dict_entry d;;

(* or, *)

let rec print_dict' =
  iter print_dict_entry;;

(* takes input from user, if int = 0 then stop *)
let rec read_dict () =
  let i = read_int () in
  if i = 0 then [] else
    let name = read_line () in
    (i, name) :: read_dict ();;

let rec read_dict () =
  try
    let i = read_int () in
    if i = 0 then [] else
      let name = read_line () in
      (i, name) :: read_dict ()
  with
    Failure "int_of_string" ->
    print_string "This is not a valid integer. Please try again. \n";
    read_dict ();;

(* outputting a file *)
(* helper functions *)

let entry_to_channel ch (k, v) =
  output_string ch (string_of_int k);
  output_char ch '\n';
  output_string ch v;
  output_char ch '\n';;

let dictionary_to_channel ch d =
  iter (entry_to_channel ch) d;;

(* main function *)

let dictionary_to_file filename dict =
  let ch = open_out filename in
  dictionary_to_channel ch dict;
  close_out ch;;

(* reading a file *)
(* helper functions *)

let entry_of_channel ch =
  let number = input_line ch in
  let name = input_line ch in
  (int_of_string number, name);;

let rec dictionary_of_channel ch =
  try
    let e = entry_of_channel ch in
    e :: dictionary_of_channel ch
  with End_of_file -> [];;

(* main function *)

let dictionary_of_file filename =
  let ch = open_in filename in
  let dict = dictionary_of_channel ch in
  close_in ch;
  dict;;

(* question 1 *)
let print_integers l =
  print_string "[";
  let rec print_integers_helper l =
    match l with
    | [] -> ()
    | [i] -> print_int i
    | h :: t -> print_int h;
                print_string "; ";
                print_integers_helper t
  in print_integers_helper l;
     print_string "]";;

(* question 2 *)
let rec read_three_ints () =
  try
    print_string "Please enter three integers, pressing Enter after each\n";
    let x = read_int () in
    let y = read_int () in
    let z = read_int () in
    (x, y, z)
  with
    Failure "int_of_string" ->
    print_string "Failed to read integers, please try again.\n";
    read_three_ints ();;

(*question 3*)

exception BadNumber;;

let rec read_dict' n =
  if n = 0 then [] else
    try
      let i = read_int () in
      let name = read_line () in
      (i, name) :: read_dict' (n - 1)
    with
      Failure "int_of_string" ->
      print_string "This is not a valid integer.\n";
      print_string "Please enter a valid integer and try again.\n";
      read_dict' n;;

let rec read_dict () =
  print_string "How many dictionary entries to input?\n";
  try
    let n = read_int () in
    if n < 0 then raise BadNumber else read_dict' n
  with
    Failure "int_of_string" ->
    print_string "Not an integer.\n";
    read_dict();
  | BadNumber ->
     print_string "Number is negative. Try again.\n";
     read_dict ();;


(*question 4*)

let rec numlist n =
  match n with
  | 0 -> []
  | _ -> numlist (n-1) @ [n];;

let write_table_channel ch n =
  iter
    (fun x ->
      iter
        (fun i ->
          output_string ch (string_of_int i);;
    ));;

(*question 5*)

let rec countlines_channel ch =
  try
    let _ = input_line ch in
    1 + countlines_channel ch
  with
    End_of_file -> 0;;

let countlines file =
  try
    let ch = open_in file in
    let result = countlines_channel ch in
    close_in ch;
    result
  with
    _ -> raise (Failure "countlines");;


                    
(*question 6*)

let rec copy_file from_ch to_ch =
  try
    output_string to_ch (input_line from_ch);
    output_string to_ch "\n";
    copy_file from_ch to_ch
  with
    End_of_file -> ();;

exception CopyFailed;;

let copy from_name to_name =
  try
    let from_ch = open_in from_name in
    let to_ch = open_out to_name in
    copy_file from_ch to_ch;
    close_in from_ch;
    close_out to_ch;
  with
    _ -> raise CopyFailed;;

    
