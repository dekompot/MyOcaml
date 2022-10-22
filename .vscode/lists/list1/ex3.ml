(*let rec replicate (x, n) = 
  if n < 0 then failwith ("negative argument" ^ (string_of_int n))
  else if n = 0 then []
  else x :: replicate(x, n - 1);;*)

  
let rec replicate (x, n) = 
  if n <= 0 then []
  else x :: replicate(x, n - 1);;

replicate('a', 3);;
replicate('a', 0);;
replicate('a', -3);;
