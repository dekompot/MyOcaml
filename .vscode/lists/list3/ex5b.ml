let mergesort pred xs = 
  let rec split_to_lists = function 
  | h::t -> [h]::split_to_lists t 
  | [] -> []
in let rec merge (xs1, xs2) = 
  match (xs1, xs2) with 
  | (h1::t1, h2::t2) when pred h2 h1 ->
     h2::merge(xs1, t2)
     (*h1::t1, otherwise it'll match an empty list in place of xs1*)
  | (h1::t1, h2::t2) -> h1::merge(t1, xs2)
  | (_, []) -> xs1
  | ([], _) -> xs2 

  in let rec merge_two = function
    | h1::h2::t -> merge(h1, h2)::merge_two t
    | h -> h
  in let rec loop xss = 
    match xss with 
    | [h] -> h
    | [] -> []
    | _ -> loop (merge_two xss)
  in loop (split_to_lists xs) ;; 
  
mergesort (<) [13; 5; 10; 8; 6; 22; 11; 3; 12; 
20; 7; 9; 14; 17; 19; 1; 2; 18];;

mergesort (fun (x1, y1) (x2, y2) -> (x1 < x2)) [(1, 0); (3, 0); 
(2, 0); (2, 1); (3, 1); (1, 1); (2, 2); (1, 2)];;