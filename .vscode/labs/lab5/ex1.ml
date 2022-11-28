type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec lfrom k = LCons(k, fun () -> lfrom(k + 1));;

let rec ltake (n, lxs) = 
  match (n, lxs) with 
  | (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x, xf)) -> x::ltake(n - 1, xf());;

let rec lsplit lxs = 
  match lxs with 
  | LNil -> (LNil, LNil) 
  | LCons(x1, xf) -> match (xf()) with 
  (*how to compute it only once???*)
                  | LCons(x2, yf) -> (LCons(x1, fun () -> fst(lsplit (yf()))), 
                                      LCons(x2, fun () -> snd(lsplit (yf()))))
                  | LNil -> (LCons(x1, fun () -> LNil), LNil);;

let (lxs1, lxs2) = lsplit(lfrom 0);;
ltake (10, lxs1);;
ltake (10, lxs2);;

let uneven_lazy_list = LCons('a', fun () -> LCons('b', fun () -> LCons('c', fun () -> LNil)));;
let (lxs1, lxs2) = lsplit uneven_lazy_list;;
ltake (10, lxs1);;
ltake (10, lxs2);;
let even_lazy_list = LCons('a', fun () -> LCons('b', fun () -> LCons('c', fun () -> LCons('d', fun () -> LNil))));;
let (lxs1, lxs2) = lsplit even_lazy_list;;
ltake (10, lxs1);;
ltake (10, lxs2);;

let (lxs1, lxs2) = lsplit LNil;;
ltake (10, lxs1);;
ltake (10, lxs2);;

