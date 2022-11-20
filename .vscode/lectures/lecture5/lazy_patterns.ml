let force (lazy v) = v;;

type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let lhd = function
| LNil -> failwith "Empty list"
| LCons(x, _) -> x

let ltl = function
| LNil -> failwith "Empty list"
| LCons(_, lazy t) -> t;;

let rec lfrom k = LCons(k, lazy (lfrom (k + 1)));;

let rec ltake = function 
| (0, _) -> []
| (_, LNil) -> []
| (n, LCons(x, lazy t)) -> x::ltake(n - 1, t);;

lhd (lfrom 3);;
ltl (lfrom 3);;

ltake (10, lfrom(3));;

let rec lmap f = function
| LNil -> LNil 
| LCons(x, lazy t) -> LCons(f x, lazy (lmap f t));;

let lsquares lxs = lmap (fun x -> x * x) lxs;;

ltake (10, lsquares (lfrom 1));;

