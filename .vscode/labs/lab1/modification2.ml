let rec sum_list list = 
  if list = [] then []
  else if List.tl list = [] then list
  else let new_val = (List.hd (list) +. (List.hd (List.tl list))) in
    (List.hd list) :: new_val:: List.tl (sum_list(List.tl list));;


sum_list [1.; 2.; 3.; 4.; 5.; 6.];;
sum_list [1.];;
sum_list [];;

(*1.::[1. + 2.; 3.; 4.]*)

(*1, 2, 3, 4*)