let rec quicksort = function 
| [] -> []
| [x] -> [x]
| xs -> let small = List.filter (fun y -> y < List.hd xs) xs 
        and large = List.filter (fun y -> y >= List.hd xs) xs  
        in quicksort small @ quicksort large;; 

quicksort([5; 5]);;

let rec quicksort' = function 
| [] -> []
| x::xs -> let small = List.filter (fun y -> y < x) xs 
          and large = List.filter (fun y -> y > x) xs 
          in quicksort' small @ (x::quicksort' large);;

quicksort'([5; 5; 5; 5]);;
quicksort'([5; 4; 3; 2]);;