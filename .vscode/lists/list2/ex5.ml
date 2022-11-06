let rec initSegment (xs, ys) = 
  match (xs, ys) with 
  | (l1, l2) when l1 = l2 -> true
  | ([], _) -> true 
  | (h1::t1, h2::t2) when h1 = h2 -> initSegment(t1, t2)
  | _ -> false;;

initSegment([1; 2; 3], [1; 2; 3; 4; 5]);;
initSegment([1; 2; 3], [1; 2; 3;]);;
initSegment([], [1; 2; 3;]);;
initSegment([1; 2; 5], [1; 2; 3; 4; 5]);;
initSegment([1; 2; 3], [1; 2]);;
initSegment(['a'; 'b'; 'c'], ['a'; 'b'; 'c'; 'd']);;