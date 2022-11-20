let f1 x y z = x y z;;

let f2 x y = function z -> x::y;;

let rec f = fun b -> f b;;

let rec f = function 
| x -> f x;;