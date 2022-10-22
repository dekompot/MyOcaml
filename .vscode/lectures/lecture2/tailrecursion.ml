let rec suc n = if n = 0 then 1 else 1 + suc(n - 1);;

suc 1000000;;

let tail_rec n =
  let rec rec_iter (n, accum) =
    if n = 0 then accum else rec_iter(n - 1, accum + 1)
  in rec_iter(n, 1);;

tail_rec 1000000;;
