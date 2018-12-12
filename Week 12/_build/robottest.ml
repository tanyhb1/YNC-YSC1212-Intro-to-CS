open Printf;;
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor

let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);  exit 1;
  ) else Sys.argv.(1);;
3
let usleep s = ignore(Unix.select [] [] [] s);;

let conn = Mindstorm.connect_bluetooth bt;;

let stop _ =
  printf "Exiting cleanly.\n";
  Motor.set conn Motor.a (Motor.speed 0);
  Motor.set conn Motor.b (Motor.speed 0);
  Mindstorm.close conn;
  exit 0;;
             
let ultra = Sensor.Ultrasonic.make conn `S4;;

Sensor.set conn `S3 `Light_active `Pct_full_scale;;

let light = Sensor.get conn `S3;;

let get_ultra () =
  Sensor.Ultrasonic.set ultra `Meas;
  Sensor.Ultrasonic.get ultra `Byte0;;

let set_speed a b =
  Motor.set conn Motor.a (Motor.speed a);
  Motor.set conn Motor.b (Motor.speed b);;

let turn_right t =
  set_speed 20 (-20);
  usleep (t *. 0.01);;

let turn_left t =
  set_speed (-20) 20;
  usleep (t *. 0.01);;

let move_forward t =
  set_speed 20 20;
  usleep (t *. 0.105);;

let move_back t =
  set_speed (-20) (-20);
  usleep (t *. 0.105);;

let rush_back t =
  set_speed (-50) (-50);
  usleep (t *. 0.042);;

let ram t =
  set_speed 50 50;
  usleep (t *. 0.042);;

let pause t =
  set_speed 0 0;
  usleep (t *. 0.05);;

let attack_box  _ =
    if (get_ultra () < 80)
    then move_forward 1.0
    else turn_left 0.1;;

let fight_box _ =
  while true do
    let light = Sensor.get conn `S3 in
    if light.scaled >= 45
    then attack_box ()
    else
      begin
        move_back 20.0; turn_left 90.0
      end
  done;;

let attack_bot  _ =
    if (get_ultra () < 80)
    then ram 1.0
    else turn_left 0.1;;

let fight_bot _ =
  while true do
    let light = Sensor.get conn `S3 in
    if light.scaled >= 45
    then attack_bot ()
    else
      begin
        rush_back 20.0; turn_left 90.0
      end
  done;;

let check _ =
  let light = Sensor.get conn `S3 in
  Printf.printf("Sensor sees %i\n") light.Sensor.scaled;;

let circle _ =
  while true do
    let light = Sensor.get conn `S3 in
    check ();
    if light.scaled >= 45
    then move_forward 1.0
    else (move_back 15.0; turn_right 90.0)
  done;;

let follow_line _ =
  let counter_right = ref 0 in
  let counter_left = ref 0 in
  while true do
    let light = Sensor.get conn `S3 in
    if light.scaled < 45
    then
      begin
        move_forward 1.0; counter_right := 0; counter_left := 0;
      end
    else if (light.scaled >= 45 && !counter_right < 45)
    then
      begin
        turn_right 1.0; incr counter_right
      end
    else if (light.scaled >= 45 && !counter_left < 180)
    then
      begin
        turn_left 1.0; incr counter_left;
      end
    else stop ()
  done;;

let move_10 _ =
  move_forward 10.0;
  pause 1.0 ;;

let turn_90 _ =
  turn_right 90.0;
  pause 1.0;;

let check_u _ =
  while true do
    print_int (get_ultra ())
  done;;
(* light.scaled < 45 is black line; light.scaled >= 45 is white *)
let rec sweep acc deg t =
  let light = ref (Sensor.get conn `S3) in
  (if ((!light).scaled < 45) then (printf ("exiting turn");)
   else (if (t > 200)
         then (if (acc > 0)
               then (set_speed (-5) 5; usleep (float_of_int ((abs acc)/40));
                     set_speed (-30) (-30); usleep 0.5; sweep acc deg 0)
               else (set_speed 5 (-5); usleep (float_of_int ((abs acc)/40));
                     set_speed (-30) (-30); usleep 0.5; sweep acc deg 0))
         else
           (if cw
            then
              (if (acc > int_of_float(0.2*.(float_of_int time)))
               then (set_speed (-16) 16; sweep (acc - 1) false (time + 1))
               else (set_speed 16 (-16); sweep (acc + 1) true (time + 1)))
            else
              (if (acc < int_of_float ((-0.2)*.(float_of_int time))) then (set_speed 16 (-16); sweep (acc + 1) true (time + 1))
               else (set_speed (-16) (16); sweep (acc - 1) false (time - 1))))));;

let maze _ =
  while true do
    let light = Sensor.get conn `S3 in
    if (light.scaled < 45) then
      (set_speed 35 35;)
    else
      a(set_speed (-20) (-20);
       usleep 0.2;
       sweep 0 true 0;
       usleep 0.05;)
done;;

(* 

Ocamlfind ocamlc -package mindstorm -annot robottest.ml -linkpkg -o robot 

./robot /dev/tty.NXT-DevB 

print_int (get_ultra());; *)


let follow_line2 _ =
  let light = Sensor.get conn `S3 in
  let counter_right = ref 0 in
  let counter_left = ref 0 in
  while true do
    if light.scaled < 45 then (move_forward 1.0; counter_right := 0; counter_left := 0)
    else if (light.scaled >= 45 && !counter_right < 90) then (turn_right 1.0; counter_right := !counter_right + 1)
    else if (light.scaled >= 45 && !counter_left < 180) then (turn_left 1.0; counter_left := !counter_left + 1)
    else stop ()
  done;;

stop ();;
