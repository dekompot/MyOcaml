(*MODYFIKACJA*)
let (>=>) l f = 
  match l with 
  | h1::h2::t -> 
    let res = List.fold_left (fun l x -> (f (List.hd l) x)::l) [f h1 h2] t
  in List.rev res 
  | _ -> [];;

let ( ** ) fl1 fl2 = Float.pow (fl1) (fl2);;

2. ** 3.;;

[2.; 1.; 3.; 2.] >=> ( ** );;

[1; 2; 1; 3; 2; 4] >=> ( * );;

[1; 2; 3; 4; 5] >=> (+);;
[1; 2; 3; 4; 5] >=> (+) >=> ( * );;

print_string "Hell"