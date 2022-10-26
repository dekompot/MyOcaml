(*1*)
let rec find l x = 
  match l with 
  | [] -> false 
  | hd::_ when hd = x -> true 
  | _::tl -> find tl x;;

let find123 = find [1; 2; 3];;

find123 1;;
find123 2;;
find123 3;;

find123 (-1);;
find123 5;;

let find_abc = find ['a'; 'b'; 'c'];;

find_abc 'c';;
find_abc 'x';;
(*2*)
let find_empty = find [];;
find_empty 1;;

let rec split3Rec xs = 
  match xs with 
  | h1::h2::h3::tl -> 
    let (xs1, xs2, xs3) = split3Rec tl in (h1::xs1, h2::xs2, h3::xs3)
  | _ -> ([], [], []);;
  
split3Rec [1; 2; 3];;
split3Rec [1; 2];;
split3Rec [1];;
split3Rec [];;
split3Rec [1; 2; 3; 4; 5; 6; 7; 8; 9];;
split3Rec [1; 2; 3; 4; 5; 6; 7; 8];;
split3Rec [1; 2; 3; 4; 5; 6; 7];;

(*3*)
let split3Tail xs = 
  let rec split3Inner (xs, xs1, xs2, xs3) = 
    match xs with 
  | h1::h2::h3::t -> split3Inner(t, h1::xs1, h2::xs2, h3::xs3)
  | _ -> (xs1, xs2, xs3)
 in split3Inner(xs, [], [], []);;

split3Tail [1; 2; 3];;
split3Tail [1; 2];;
split3Tail [1];;
split3Tail [];;
split3Tail [1; 2; 3; 4; 5; 6; 7; 8; 9];;
split3Tail [1; 2; 3; 4; 5; 6; 7; 8];;
split3Tail [1; 2; 3; 4; 5; 6; 7];;