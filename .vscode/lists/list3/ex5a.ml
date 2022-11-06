let insertionsort pred xs = 
  let rec insert ys x = 
    match ys with 
    | h::t when pred x h -> x::h::t
    | h::t -> h::insert t x 
    (*!!! forgot about this*)
    | [] -> [x]
  in let rec sort_inner ys acc = 
    match ys with 
    | h::t -> sort_inner t (insert acc h)
    | [] -> acc
  in sort_inner xs [];;


insertionsort (<) [4; 3; 5; 3; 2; 4; 5];;

insertionsort (fun (x1, y1) (x2, y2) -> x1 < x2) 
[(1, 2); (0, 3); (0, 1); (1, 3)];;


