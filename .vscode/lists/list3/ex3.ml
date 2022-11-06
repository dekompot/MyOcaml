let sumProd = function 
  | xs -> List.fold_left (fun (s, p) x -> (s + x, p * x)) (0, 1) xs;;

sumProd [1; 2; 3; 4];;
sumProd [];;