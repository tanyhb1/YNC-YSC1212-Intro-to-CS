(* File to stock the Museum.  

   Aquinas Hobor
*)

open Printf;;

let oc = open_out "Museum.cat";;

fprintf oc "cobra venom\n0.2104\n1208.8\nmilk\n1.3\n8.\nwater\n202.1\n25.2\nmercury\n0.72\n28.8\nAB positive\n12.21\n248.1\nhoney\n3.14\n8.25\nvinegar\n0.12\n12.8\nolive oil\n1.59\n2.653\nChampagne\n0.243\n300.11\nprinter ink\n0.01\n221.1\ngasoline\n71.17\n838.38\nCoca-Cola\n7.14\n62.8\nplasma\n20.\n1434.\nhydrofluoric acid\n4.54\n55.44\nbromine\n98.8\n101.1\ngrapefruit juice\n19.819\n49.1981\ninsulin\n0.25\n408.8\negg white\n4.21\n12.48\n";;

close_out oc;;
