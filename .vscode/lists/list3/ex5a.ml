let insertionsort pred xs = 
  let rec insert x = function 
    | h::t when pred x h -> x::h::t
    | h::t -> h::insert x t
    | [] -> [x]
  in let rec loop acc = function
    | h::t -> loop (insert h acc) t
    | [] -> acc
  in loop [] xs;;

insertionsort (<) [13; 5; 10; 8; 6; 22; 11; 3; 12; 20; 7; 9; 14; 17; 19; 1; 2; 18];;

insertionsort (<) ['d'; 'c'; 'f'; 'q'; 'a'; 'e'];;

insertionsort (fun (x1, y1) (x2, y2) -> (x1 < x2)) [(1, 0); (3, 0); (2, 0); (2, 1); (3, 1); (1, 1); (2, 2); (1, 2)];;