type 'a t = {n: int; mutable r: int; mutable f: int; mutable arr: 'a array}


let enqueue' (e, q) = 

  if (q.r + 1) mod q.n = q.f then failwith "full queue" 
  else if q.r = q.f then 
    begin q.arr <- Array.make q.n e ; q.r <- (q.r + 1) mod q.n ; print_string "case 2" end
  else q.arr.(q.r) <- e; q.r <- (q.r + 1) mod q.n; print_string "case 3";;

let enqueue (e, q) = 
  match (q.f, q.r) with 
  | (f, r) when f = (r + 1) mod q.n -> failwith "full queue"
  | (f, r) when f = r -> begin q.arr <- Array.make q.n e ; q.r <- (q.r + 1) mod q.n ; print_string "case 2" end
  | _ -> q.arr.(q.r) <- e; q.r <- (q.r + 1) mod q.n; print_string "case 3";;


let q = {n=4; r=0; f=0; arr=[||]};;

enqueue (1, q);;
q;;


