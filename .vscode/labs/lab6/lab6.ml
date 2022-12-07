let pascal n = 
  if n < 1 then failwith ("Illegal Argument = "^(string_of_int n))
  else 
  let prev_row = Array.make (2 * n + 1) 1; 
  and curr_row = Array.make (2 * n + 1) 1; 
  and r = ref 2 and upper = ref 2 in 
  
  while !r <= n do 
    upper := 2 * !r - 2;
    for i = 2 to !upper do 
      curr_row.(i) <- prev_row.(i - 2) + prev_row.(i - 1) + 
                      prev_row.(i)
    done;
    
    for i = 0 to !upper do
      prev_row.(i) <- curr_row.(i)
    done;

    r := succ !r; 

  done;
  curr_row;; 
  
pascal 0;;
pascal 1;;
pascal 2;;
pascal 3;;
pascal 4;;
pascal 5;;