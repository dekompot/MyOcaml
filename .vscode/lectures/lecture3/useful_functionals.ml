let rec fold_right f xs acc = 
  match xs with 
  | [] -> acc
  | h::t -> f h (fold_right f t acc);;

let sum_list xs = fold_right (fun x y -> x + y) xs 0;;

sum_list [1; 2; 4; 5];;

let rec fold_left f xs acc = 
  match xs with 
  | [] -> acc 
  | h::t -> fold_left f t (f h acc);;

let mult_list xs = fold_left (fun x y -> x * y) xs 1;;

mult_list [1; 2; 2; 3];;

let sumlist = List.fold_left (+) 0;;

sumlist [1; 2; 3; 4];;

let prodlist = List.fold_left ( * ) 1;;

prodlist [1; 2; 2; 3];;

let flatten xss = List.fold_left (@) [] xss;;

flatten [[2; 3]; [4; 5]];;

let implode = List.fold_left (^) "";;

implode ["Litwo"; " ojczynzo"; " moja"];;

