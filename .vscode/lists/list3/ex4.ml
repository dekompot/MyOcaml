let rec quicksort = function 
| [] -> []
| [x] -> [x]
| xs -> let small = List.filter (fun y -> y < List.hd xs) xs 
        and large = List.filter (fun y -> y >= List.hd xs) xs  
        in quicksort small @ quicksort large;; 

quicksort([5; 5]);;
quicksort([3; 3; 2; 4; 5; 3; 2; 5; 4]);;

let rec quicksort' = function 
| [] -> []
| x::xs -> let small = List.filter (fun y -> y < x) xs 
          and large = List.filter (fun y -> y > x) xs 
          in quicksort' small @ (x::quicksort' large);;

quicksort'([5; 5; 5; 5]);;
quicksort'([5; 4; 3; 2]);;
quicksort'([3; 3; 2; 4; 5; 3; 2; 5; 4]);;

let rec quicksort'' = function 
| [] -> []
| [x] -> [x]
| xs -> let small = List.filter (fun y -> y < List.hd xs) xs 
        and large = List.filter (fun y -> y > List.hd xs) xs 
        and equal = List.filter (fun y -> y = List.hd xs) xs 
        in quicksort'' small @ equal @ quicksort'' large;; 

quicksort''([5; 5; 5; 5]);;
quicksort''([13; 5; 10; 8; 6; 22; 11; 3; 12; 20; 7; 9; 14; 17; 19; 1; 2; 18]);;
quicksort''([3; 3; 2; 4; 5; 3; 2; 5; 4]);;

let rec quicksort4 = function 
| [] -> []
| x::xs -> let small = List.filter (fun y -> y < x) xs 
          and large = List.filter (fun y -> y > x) xs 
          and equal = List.filter (fun y -> y = x) xs 
          in quicksort4 small @ equal @ (x::quicksort4 large);;

quicksort4([13; 5; 10; 8; 6; 22; 11; 3; 12; 20; 7; 9; 14; 17; 19; 1; 2; 18]);;
quicksort4([3; 3; 2; 4; 5; 3; 2; 5; 4]);;