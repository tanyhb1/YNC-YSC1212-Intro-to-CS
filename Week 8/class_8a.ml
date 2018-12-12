(* Class 8a *)
(* Game *)

type state = int;;
type move = One | Two;;

exception BadMove;;
let update_state s m =
  let delta = match m with One -> 1 | Two -> 2 in
  let newstate = s - delta in
  if newstate < 0 then raise BadMove else newstate;;

let dumbest_play s = One;;
let fairly_dumb_play s = if s = 2 then Two else One;;

let rec plan_strategy s =
  match s with
  | 0 -> None
  | 1 -> Some One
  | 2 -> Some Two
  | _ ->
     (* suppose i took 1, does my opponent have a winning strategy? *)
     match plan_strategy (s - 1) with
     | None -> Some One
     | Some _ ->
        (* if i took 1, opponent would have a winning strategy. so, let's try taking 2 *)
        match plan_strategy (s - 2) with
        | None -> Some Two
        (* even if i took 1 or 2 i'll still lose so i just give up*)
        | Some _ -> None;;

plan_strategy 3;;
plan_strategy 4;;

(* we lost so just stall for time Clap *)
let rec best_play s =
  match plan_strategy s with
  | Some o -> o
  | None 0 -> if s mod 2 = 0 then One else Two;;

type player = Sulla | Marius;;

let flip_player p = match p with Sulla -> Marius | Marius -> Sulla;;

let play_game strat1 strat2 game_state =
  let rec helper state turn =
    if state = 0 then flip_player turn
    else let strat = match turn with Sulla -> strat1 | Marius -> strat2
         in helper (update_state state (strat state)) (flip_player turn)
  in helper game_state Sulla;;

play_game dumbest_play dumbest_play 3;;
play_game fairly_dumb_play dumbest_play 3;;

type result = Moves of move | Loses;;
type game_record = {plyr : player; state: state; res: Loses};;

let gr: game_record = 
