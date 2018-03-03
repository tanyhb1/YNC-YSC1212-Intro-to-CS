(* Class 5b *)

(*fast enqueue o(n), slow dequeue o(n**2) *)

let enqueue x q =
  x :: q;;

let rec rev_helper l acc = match l with [] -> acc | h :: t -> rev_helper t (h :: acc);;
let rev l = rev_helper l [];;


let dequeue q =
  let rev_q = rev q in
  match rev_q with
  | [] -> raise Not_found
  | h :: t -> (h, rev t);;

let emptyq = [];;
let q1 = enqueue 1 emptyq;;
let q2 = enqueue 2 q1;;
let q3 = enqueue 3 q2;;
let q4 = enqueue 4 q3;;

let (firstitem,q5) = dequeue q4;;


let rec dequeue q =
  match q with
  | [] -> raise Not_found
  | [x] -> ([], x)
  | h :: t ->
     match dequeue_helper t with
       (first_item, rest_of_list) -> (first_item, h :: rest_of_list);;

(*slow enqueue o(n), fast dequeue o(n) *)

let enqueue2 x q = q @ [x];;
let dequeue2 q =
  match q with
  | [] -> raise Not_found
  | h :: t -> (h, t);;

let emptyq = [];;
let q1 = enqueue2 1 emptyq;;
let q2 = enqueue2 2 q1;;
let q3 = enqueue2 3 q2;;
let q4 = enqueue2 4 q3;;

let (firstitem,q5) = dequeue2 q4;;

(* using stack enqueue o(n) dequeue o(n) but in most instances using both enqueue and dequeue it is faster as it will constantly be o(n)*)

let enqueue x q =
  match q with (inbox, outbox) ->
    (x :: inbox, outbox);;

let rec dequeue q =
  match q with (inbox, outbox) ->
    match outbox with
    | item :: rest_of_outbox -> (item, (inbox, rest_of_outbox))
    | [] -> (match inbox with
             | [] -> raise Not_found
             | _ -> dequeue ([], rev inbox));;

let emptyq = ([], []);;
let q1 = enqueue 1 emptyq;;
let q2 = enqueue 2 q1;;
let q3 = enqueue 3 q2;;
let q4 = enqueue 4 q3;;

let (firstitem,q5) = dequeue q4;;
