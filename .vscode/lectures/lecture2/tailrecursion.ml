let rec suc n = if n = 0 then 1 else 1 + suc(n - 1);;

suc 1000000;;

let tail_rec n =
  let rec rec_iter (n, accum) =
    if n = 0 then accum else rec_iter(n - 1, accum + 1)
  in rec_iter(n, 1);;

tail_rec 1000000;;

let rec mul n m =
  if m = 1 then n 
  else n + mul n (m - 1);;

mul 2 3;;

let mul_ n m = 
  let rec mul_inner n m acc = 
    if m = 0 then acc 
    else mul_inner n (m - 1) (acc + n)
  in mul_inner n m 0;;
  
mul_ 2 3;;
mul_ 2 600000;;

