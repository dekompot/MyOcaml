(*Calculates sum[k:0->m] f(k)*)
let sigma f n = 
  let rec sum (i, acc) = 
    if i = n then acc 
    else sum (i + 1, acc +. f (i + 1))
  in sum (0, f 0);;

let value = if true then 0 else 1 / 0;;

sigma (fun k -> float(k * k))(4);;

(*Calculated sum[i:0->n][j:0->m] (i + j)*)
let sigma2 n m = sigma (fun i -> sigma (fun j -> float(i + j))(m)) (n);;

sigma2 3 4;;

let sigma3 f n m = sigma (fun i -> sigma (fun j -> f i j)(m))(n);;

(*Why does this have float type - because the sigma function
   returns float type*)
sigma3 (fun k l -> float(k + l))(3)(4);;

(*Calculates sum[i:0->m][j:0->n] g(i, j)*)
let sigma4 g m n =
  sigma (fun i -> sigma (fun j -> g i j)(n)) (m);;

sigma4 (fun k l -> float(k + l)) 3 4;;