(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

let (>=>) l f = 
  let rec apply_left l' = 
    match l' with 
    | h1::h2::t -> let res = f h1 h2 in res::apply_left (res::t)
    | _ -> [] 
  in apply_left l;;

let max a b = if a > b then a else b;;

[1; 2; 1; 3; 2; 4] >=> max;;

[1; 2; 1; 3; 2; 4] >=> max >=> ( * );;

[1; 2; 1; 3; 2; 4] >=> max >=> ( * ) >=> (+);; 

let ( ** ) fl1 fl2 = Float.pow (fl1) (fl2);;

2. ** 3.;;

[2.; 1.; 3.; 2.] >=> ( ** );;
  
["a"; "b"; "c"; "d"] >=> (^);; 

[1] >=> (+);;

[] >=> (+);;
[] >=> ( *. );;

let rec collapse l f = 
  match l with 
  | h1::h2::t -> collapse (l >=> f) f
  | h1::[] as ys -> ys
  | _ -> [];;

collapse [1; 2; 3; 4; 5] (+);; 

[1; 2; 3; 4; 5] >=> (+) >=> (+) >=> (+) >=> (+) ;;

["a"; "b"; "c"; "d"] >=> (^);;
["a"; "b"; "c"; "d"] >=> (^) >=> (^);;
["a"; "b"; "c"; "d"] >=> (^) >=> (^) >=> (^);;
(*??? empty list is ok?*)
["a"; "b"; "c"; "d"] >=> (^) >=> (^) >=> (^) >=> (^);;

collapse ["a"; "b"; "c"; "d"] (^);;
(*[2.; 8.] -> [256.]*)
collapse [2.; 1.; 3.] ( ** );;
collapse [] (+);;

  