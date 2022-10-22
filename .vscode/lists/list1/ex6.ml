let rec listLength l = 
  if l = [] then 0
  else 1 + listLength(List.tl l);;

listLength [1; 2; 3; 4; 5];;
listLength ['a'];;
listLength [];;

let rec listLength' l = 
  match l with 
  | [] -> 0
  | _::t -> 1 + listLength' t;;

listLength' [1; 2; 3; 4; 5];;
listLength' ['a'];;
listLength' [];;