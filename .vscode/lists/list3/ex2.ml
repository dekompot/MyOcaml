(*NO SYNTACTIC SUGAR VERSION*)

let curry3 = function f -> function x -> function y -> function z -> f(x, y, z);;

let max3 = function (x, y, z) -> 
  if x < y then (if y > z then y else z) else if x > z then x else z;;

max3 (1, 2, 3);;
max3 (1, 3, 2);;
max3 (3, 2, 1);;
max3 (3, 1, 2);;
max3 (2, 1, 3);;
max3 (2, 3, 1);;

curry3 max3 1 2 3;;

curry3 max3 1 2;;

let uncurry3 = function f -> function (x, y, z) -> f x y z;;


let min3 = function x -> function y -> function z -> 
  if x < z then if x < y then x else y else if z < y then z else y;;

min3 1 2 3;;
min3 1 3 2;;
min3 3 1 2;;
min3 3 2 1;;
min3 2 1 3;;
min3 2 3 1;;

uncurry3 min3 (1, 2, 3);;

(*SYNTACTIC SUGAR VERSION*)
let curry3 f x y z = f(x, y, z);;

curry3 max3 1 2 3;;

let uncurry3 f (x, y, z) = f x y z;;

uncurry3 min3 (1, 2, 3);;