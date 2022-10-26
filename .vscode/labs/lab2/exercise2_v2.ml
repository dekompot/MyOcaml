let rec split2Rec xs = 
  match xs with
  |  [] -> ([], [])
  |  h1::h2::tl -> let (xs1, xs2) = split2Rec tl in (h1::xs1, h2::xs2)
  |  h::tl -> let (xs1, xs2) = split2Rec tl in (h::xs1, xs2)
;;

split2Rec [1; 2; 3; 4; 5; 6];;
split2Rec [1; 2; 3; 4; 5];;
split2Rec [1; 2];;

let xs = [1; 2];;
let h1::h2::tl = xs;;

let rec split3Rec xs = 
  match xs with 
  | h1::h2::h3::tl -> 
    let (xs1, xs2, xs3) = split3Rec tl in (h1::xs1, h2::xs2, h3::xs3)
  | h1::h2::tl -> 
    let (xs1, xs2, xs3) = split3Rec tl in (h1::xs1, h2::xs2, xs3)
  | h1::tl -> 
    let (xs1, xs2, xs3) = split3Rec tl in (h1::xs1, xs2, xs3)
  | [] -> ([], [], []);;

  let split3Tail xs = 
    let rec split3Inner (xs, xs1, xs2, xs3) = 
      match xs with 
    | h1::h2::h3::t -> split3Inner(t, h1::xs1, h2::xs2, h3::xs3)
    | h1::h2::t -> split3Inner(t, h1::xs1, h2::xs2, xs3)
    | h1::t -> split3Inner(t, h1::xs1, xs2, xs3)
    | [] -> (xs1, xs2, xs3)
   in split3Inner(xs, [], [], []);;