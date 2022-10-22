let rec count (x, l) = 
  if l = [] then 0
  else (if List.hd l = x then 1 else 0) + count(x, List.tl l);;

count('a', ['a'; 'c'; 'a']);;
count('a', ['c'; 'd'; 'e']);;
count([1], [[1]; [1; 3]; [1]]);;
count(1, []);;

let rec count' (x, l) = 
  match l with 
  | [] -> 0
  | h::t when h = x -> 1 + count'(x, t)
  | _::t -> count'(x, t);;

count'('a', ['a'; 'c'; 'a']);;
count'('a', ['c'; 'd'; 'e']);;
count'([1], [[1]; [1; 3]; [1]]);;
count'(1, []);;