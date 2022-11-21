type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons(k, lazy (lfrom(k + 1)));;

let rec ltake (n, lxs) = 
  match (n, lxs) with 
  | (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x, lazy xf)) -> x::ltake(n - 1, xf);;

let rec lcombine (lxs, lys) f = 
  match (lxs, lys) with 
  | (LCons(x, lazy xf), LCons(y, lazy yf)) -> LCons(f (x, y), lazy (lcombine(xf, yf) f))
  | _ -> LNil;;

ltake (10, lcombine (lfrom 1, lfrom 1) (fun (x, y) -> x * y));; 
ltake (10, lcombine (lfrom 1, lfrom 2) (fun (x, y) -> x + y));;

let str_lazy_list1 = LCons("a", 
                  lazy (LCons("b", 
                  lazy (LCons ("c", 
                  lazy (LCons("d", 
                  lazy LNil)))))));;


let str_lazy_list2 = LCons("x", 
                  lazy (LCons("y", 
                  lazy (LCons ("z", 
                  lazy (LCons("v", 
                  lazy LNil)))))));;

ltake (10, lcombine (str_lazy_list1, str_lazy_list2) (fun (s1, s2) -> s1 ^ s2));;
ltake (10, lcombine(LNil, lfrom 2) (fun (x, y) -> x + y));;
ltake (10, lcombine(LNil, LNil) (fun (x, y) -> x + y));;