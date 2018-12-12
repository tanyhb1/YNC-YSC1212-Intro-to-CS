(* Class 10a / Chapter 13 *)

let x = ref 0;;
let p = !x;;
x := 50;;
let q = !x;;

let swap a b =
  let t = !a in
  a := !b;
  b := t;;

for x = 1 to 5 do
  print_int x;
  print_newline ()
done;
;;


let smallest_pow2 x =
  let t = ref 1 in
  while !t < x do
    t := !t * 2
  done;
  !t;;

(* references/while/for wrt in/out*)

let channel_statistics in_channel =
  (*setting up storage variable*)
  let lines = ref 0 in
  try
    while true do
      (*for each line in in-channel, add 1 to lines *)
      let line = input_line in_channel in
      lines := !lines + 1
    done
  with
    (*when EOF reached, print sentence stating number of lines *)
    End_of_file ->
    print_string "There were ";
    print_int !lines;
    print_string " lines.\n";
;;


let file_statistics name =
  (*open channel*)
  let channel = open_in name in
  try
    (* run the helper function to count number of lines *)
    channel_statistics channel;
    close_in channel
  with
    (* close channel *)
    _ -> close_in channel;;

(* expanding channel_statistics to include counting characters in the sentences *)
let channel_statistics in_channel =
  (*setting up storage variable*)
  let lines = ref 0 in
  let characters = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  try
    while true do
      (*for each line in in-channel, add 1 to lines *)
      let line = input_line in_channel in
      lines := !lines + 1;
      characters := !characters + String.length line;
      String.iter
        (fun c ->
          match c with
          |'.'|'?'|'!' -> sentences := !sentences + 1
          | ' ' -> words := !words + 1
          | _ -> ()) line      
    done
  with
    (*when EOF reached, print sentence stating number of lines *)
    End_of_file ->
    print_string "There were ";
    print_int !lines;
    print_string " lines, making up ";
    print_int !characters;
    print_string " characters with ";
    print_int !words;
    print_string " words in ";
    print_int !sentences;
    print_string " sentences.\n";
;;


(* creating arrays *)
(* declaration, where 10 is the size *)
let myArray = Array.make 10 0;;
(* or *)
let a = [|1;2;3;4;5|];;

(*accessing arrays is constant time *)
a.(0);;

(*updating values is constant time as well *)
a.(4) <- 100;;
a;;

(*finding length of array is constant, since it is declared before the array was created instead 
of being found through iteration *)
Array.length a;;

(*Array.make x y builds an array of length x, initialized with y *)
Array.make 6 true;;
Array.make 10 'a';;
Array.make 3 [|5;3;1|];;

(*making a histogram *)

let print_histogram arr =
  print_string "character frequencies: ";
  print_newline ();
  for x = 0 to 255 do
    if arr.(x) > 0 then
      begin
        print_char (char_of_int x);
        print_string "'(character number ";
        print_int x;
        print_string ") the count is ";
        print_int arr.(x);
        print_string ".\n";
      end
  done
;;


let channel_statistics in_channel =
  (*setting up storage variable*)
  let lines = ref 0 in
  let characters = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  let histogram = Array.make 256 0 in
  try
    while true do
      (*for each line in in-channel, add 1 to lines *)
      let line = input_line in_channel in
      lines := !lines + 1;
      characters := !characters + String.length line;
      String.iter
        (fun c ->
          match c with
          |'.'|'?'|'!' -> sentences := !sentences + 1
          | ' ' -> words := !words + 1
          | _ -> ()) line;
      String.iter
        (fun c ->
          let i = int_of_char c in
          histogram.(i) <- histogram.(i) + 1) line;
    done
  with
    (*when EOF reached, print sentence stating number of lines *)
    End_of_file ->
    print_string "There were ";
    print_int !lines;
    print_string " lines, making up ";
    print_int !characters;
    print_string " characters with ";
    print_int !words;
    print_string " words in ";
    print_int !sentences;
    print_string " sentences.\n";
;;


(* question 1 *)
let x = ref 1 in
    let y = ref 2 in
    x := !x + !y;
    y := !x + !y;
    !x + !y
;;

(* question 2 *)
[ref 5; ref 5];;
let x = ref 5 in
    [x; x];;
x := 10;;
[x; x];;

(* question 3 *)
for x = 1 to 5 do
  print_int x;
  print_newline ()
done;
;;
let rec printer n =
  if n > 0 then
    begin
      print_int n;
      print_newline ();
    end
  else printer (n - 1);;

let rec forloop f n m =
  if n <= m then
    begin
      f n;
      forloop f (n + 1) m;
    end
;;

         
(*question 4*)
[|[1;2;3]; [4;5;6]|];;
[|1;2;3|].(2);;
[|1;2;3|].(2) <- 4;;

(*question 5*)
let sum arr =
  let stor = ref 0 in
  for x = 0 to (Array.length arr) - 1 do
    stor := !stor + arr.(x);
  done;
  !stor;
;;

(*question 6*)
let rev arr =
  let len = Array.length arr in
  for x = 0 to len / 2 do
    let tmp = arr.(x) in
    arr.(x) <- arr.(len-x-1);
    arr.(len-x-1) <- tmp;
  done;
  arr;
;;

(*question 7*)
let table n =
  let arr = Array.make (n * n) 0 in
  let counter = ref 0 in
  for x = 0 to n - 1 do
    for y = 1 to n do
      arr.(!counter) <- y * (x + 1) ;
      counter := !counter + 1;
    done;
  done;
  arr;
;;
