let ff a b = b;;
let f a = ff a a;;

let rec f1 a = f2 a and f2 b = f1 b;;

f1;;
(*'a -> 'b*)
let rec f a = f a;;