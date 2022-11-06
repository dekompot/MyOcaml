let curry f x y = f(x, y);;

let plus(a, b) = a + b;;

let plus_c = curry plus;;

plus_c 1 2;;

let uncurry f (x, y) = f x y;;

let plus_u a b = a + b;;

let plus_cc = uncurry plus_u;;

plus_cc(1, 2);;
let concat xs = List.tl xs @ List.tl xs;; 

concat [1; 2; 3; 4];;

(*let (^) p t = p + t;;*)

let f x y = x ^ y;;