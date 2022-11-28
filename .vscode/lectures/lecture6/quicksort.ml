let swap tab i j = 
  let aux = tab.(i) in tab.(i) <- tab.(j); tab.(j) <- aux;;

let choose_pivot tab m n = tab.((m + n) / 2);;

let parition tab l r = 
  let i = ref l and j = ref r and pivot = choose_pivot tab l r
  in while !i <= !j do 
    while tab.(!i) < pivot do incr i done;
    while tab.(!j) > pivot do decr j done;
    if !i <= !j 
      then (swap tab (!i) (!j); incr i; decr j)
  done;
  (!i, !j)
;;

parition [|1; 3; 2; 4; 5; 3|] 0 5;;

let rec quick tab l r = 
  if l < r then 
    let (i, j) = parition tab l r
  (*this is the 3rd improvement - first sort the shorther array*)
in if j-l < r-i 
  then let _ = quick tab l j in quick tab i r
  else let _ = quick tab i r in quick tab l j
else ();;

let quicksort tab = quick tab 0 (Array.length tab - 1);;

let t1 = [|4;8;1;12;7;3;1;9|];;

quicksort t1;;

t1;;