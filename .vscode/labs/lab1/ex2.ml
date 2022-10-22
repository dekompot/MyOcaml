let rec remove (l, i) = 
  if l = [] then []
  else if i = 0 then List.tl l
  else (List.hd l) :: remove(List.tl l, i - 1);;

(*valid arguments*)
remove ([1; 2; 3; 4; 5], 0);;
remove ([1; 2; 3; 4; 5], 4);;
remove ([1; 2; 3; 4; 5], 2);;

(*1-element list*)
remove([1], 0);;
remove([1], 1);;
remove([1], -1);;

(*invalid arguments*)
remove ([1; 2; 3; 4; 5], -1);;
remove ([1; 2; 3; 4; 5], 5);;

(*empty list*)
remove ([], 0);;
remove ([], -1);;
remove ([], 4);;