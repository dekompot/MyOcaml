let rec sqrList l = 
  if l = [] then []
  else let h = List.hd l in 
  (h * h) :: sqrList(List.tl l);;

sqrList [1; 2; 3; 4];;
sqrList [];;

let rec sqrList l = 
  match l with 
  | [] -> []
  | h::t -> (h * h)::sqrList(t);;

sqrList [1; 2; 3; 4];;
sqrList [];;
