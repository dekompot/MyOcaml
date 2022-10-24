(*Currying is a transformation of n-argument function
   to n 1-argument functions*)

let f_u (x, y, z) = z;;

let curry3 f x y z = f(x, y, z);;

curry3 f_u;;

let f_c x y z = z;;

let uncurry3 f (x, y, z) = f x y z;;

uncurry3 f_c;;