let rec find l x = 
  match l with 
  | [] -> false 
  | hd::tl when hd = x -> true 
  | hd::tl -> find tl x;;

let find123 = find [1; 2; 3];;

find123 1;;
find123 2;;
find123 3;;

find123 (-1);;
find123 5;;

let find_abc = find ['a'; 'b'; 'c'];;

find_abc 'c';;
find_abc 'x';;

let find_empty = find [];;
find_empty 