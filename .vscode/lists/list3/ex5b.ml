let mergesort pred xs = 
  let rec split_to_list = function 
  | h::t -> [h]::split_to_list t 
  | [] -> []
in let rec merge (xs1, xs2) = 
  match (xs1, xs2) with 
  | (h1::t1, h2::t2) when pred h1 h2 ->
     h1::merge(t1, xs2)
  | (h1::t1, h2::t2) -> h2::merge(xs1, t2)
  | ([], _) -> xs2 
  | (_, []) -> xs1
  in let rec mergeinner xss = 
    match xss with 
    | h1::h2::t -> merge(h1, h2)::mergeinner t
    | h -> h 
  in mergeinner (split_to_list xs);;

mergesort (<) [13; 5; 10; 8; 6; 22; 11; 3; 12; 20; 7; 9; 14; 17; 19; 1; 2; 18];;