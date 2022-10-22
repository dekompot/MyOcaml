let split3Tail xs = 
  let rec split3Inner (xs, xs1, xs2, xs3) = 
    match xs with 
  | h1::h2::h3::t -> split3Inner(t, h1::xs1, h2::xs2, h3::xs3)
  | h1::h2::t -> split3Inner(t, h1::xs1, h2::xs2, xs3)
  | h1::t -> split3Inner(t, h1::xs1, xs2, xs3)
  | [] -> (xs1, xs2, xs3)
 in split3Inner(xs, [], [], []);;

  split3Tail [1; 2; 3];;
  split3Tail [1; 2];;
  split3Tail [1];;
  split3Tail [];;
  split3Tail [1; 2; 3; 4; 5; 6; 7; 8; 9];;
  split3Tail [1; 2; 3; 4; 5; 6; 7; 8];;
  split3Tail [1; 2; 3; 4; 5; 6; 7];;