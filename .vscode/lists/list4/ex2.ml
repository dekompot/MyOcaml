let ff a b = b;;
let f a = ff a a;;

let rec f1 a = f2 a and f2 b = f1 b;;

f1;;
(*'a -> 'b*)
let rec f a = f a;;

let f1 x = List.hd [];;

let f a = raise Not_found;;