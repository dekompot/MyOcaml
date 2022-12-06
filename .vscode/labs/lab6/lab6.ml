let copy source destination = 
  for i = 0 to min(Array.length destination) (Array.length source) - 1 do 
    destination.(i) <- source.(i)
  done;;

let pascal n = 
  let prev_row = Array.make (2 * n + 1) 1; 
  and curr_row = Array.make (2 * n + 1) 1; 
  and r = ref 2 in 
  while !r <= n do 
    for i = 2 to 2 * !r - 2 do 
      curr_row.(i) <- prev_row.(i - 2) + prev_row.(i - 1) + 
                      prev_row.(i)
    done;
    r := succ !r; copy curr_row prev_row
  done;
  curr_row;; 
  
pascal 1;;
pascal 2;;
pascal 3;;
pascal 4;;
pascal 5;;