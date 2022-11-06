let rotations2 l =
  let rec rotations_helper(v, l) = 
    match l with 
    | h1::h2::t -> if h1 *. h2 < 0. then rotations_helper (v *. h1 /. h2, h2::t) else rotations_helper (v, h2::t) 
    | h1::_ -> v
    | [] -> 0.
  in rotations_helper(1., l);;
;;

rotations2 [-40.; 10.; 50.; 20.; -10.; -20.; -30.; 60.];;
rotations2 [-40.; 10.; 50.; 20.; -10.];;
rotations2 [20.; 10.; -10.; -15.; -40.; 20.];;
rotations2 [];;