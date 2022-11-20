type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let lhd = function 
| LNil -> failwith "lhd"
| LCons (x, _) -> x;;

let ltl = function 
| LNil -> failwith "ltl"
| LCons (_, xf) -> xf();;

let rec lfrom k = LCons(k, fun () -> lfrom(k + 1));;

let rec ltake (n, lxs) = 
  match (n, lxs) with 
  | (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x, xf)) -> x::ltake(n - 1, xf());;

ltake(5, lfrom 30);;

let rec to_lazy_list = function
| [] -> LNil 
| h::t -> LCons(h, fun () -> to_lazy_list t);;

let ll = to_lazy_list [1; 2; 3; 4];;

ltake (4, ll);;

let rec (@$) lxs1 lxs2 = 
  match lxs1 with 
  | LNil -> lxs2
  | LCons(x, xf) -> LCons(x, fun () -> xf() @$ lxs2);;

let lxs1 = to_lazy_list [1; 2];;

let lxs2 = lfrom 3;;

ltake (10, lxs1 @$ lxs2);;

let rec lmap f lxs = 
  match lxs with 
  | LNil -> LNil 
  | LCons(x, xf) -> LCons(f x, fun () -> lmap f (xf()))

let sqr_llist = lmap (fun x -> x * x);;

ltake (10, sqr_llist(lfrom 2));;

(*This will never stop if none of the elements fullfills the predicate*)
let rec lfilter pred lxs = 
  match lxs with 
  | LNil -> LNil 
  | LCons(x, xf) -> if pred x then LCons(x, fun () -> lfilter pred (xf())) else lfilter pred (xf());;

let even x = x mod 2 = 0;;

ltake (10, lfilter even (lfrom 2));;

let rec liter f x = LCons(x, fun () -> liter f (f x));;

(*ltake (10, (lfilter (fun x -> x < 0) (lfrom 3)));;*)

