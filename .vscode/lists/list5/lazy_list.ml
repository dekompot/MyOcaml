type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons(k, lazy (lfrom (k + 1)));;

let rec to_lazy_list = function
| [] -> LNil 
| h::t -> LCons(h, lazy (to_lazy_list t));;

let rec ltake = function
| (0, _) -> [] 
| (_, LNil) -> [] 
| (n, LCons(x, lazy t)) -> x::ltake(n - 1, t);;

ltake (5, lfrom 4);;

let rec lzip (lxs, lys) = 
  match (lxs, lys) with 
  | (LCons(h1, lazy t1), LCons(h2, lazy t2)) -> LCons((h1, h2), lazy (lzip (t1, t2)))
  | _ -> LNil;;

ltake (5, (lzip (lfrom 3, lfrom 5)));;
let plxs = (lzip (lfrom 3, lfrom 5))

let rec lunzip plxs = 
  print_string "eval";
  match plxs with 
  | LCons((h1, h2), lazy t) -> (LCons(h1, lazy(fst(lunzip t))), 
                                LCons(h2, lazy(snd(lunzip t))))
  | _ -> (LNil, LNil);;

let (lxs1, lxs2) = lunzip plxs;;
(ltake (4, lxs1), ltake (5, lxs2));;

(*ex 1*)

let lrepeat k lxs = 
  let rec inner n lxs = 
    match lxs with 
    | LCons(h, lazy t) -> if n = 0 then inner k t
    else LCons(h, lazy (inner (n - 1) lxs))
    | LNil -> LNil
  in inner k lxs;;

ltake (14, (lrepeat 3 (lfrom 3)));;
ltake (10, (lrepeat 3 (LCons('a', lazy (LCons('b', lazy (LCons('c', lazy LNil))))))));;
ltake (10, (lrepeat 3 LNil));;

let lrepeat2 k lxs = 
  let rec inner n lxs = 
    match lxs with 
    | LCons(h, lazy t) when n = 0 -> inner k t
    | LCons(h, lazy t) -> LCons(h, lazy (inner (n - 1) lxs))
    | LNil -> LNil
  in inner k lxs;;

ltake (14, (lrepeat2 3 (lfrom 3)));;
ltake (10, (lrepeat2 3 (LCons('a', lazy (LCons('b', lazy (LCons('c', lazy LNil))))))));;
ltake (10, (lrepeat2 3 LNil));;

let lfib = 
  let rec inner (n1, n2) = LCons(n1, lazy (inner(n2, n1 + n2)))
in inner(0, 1);;

ltake (10, lfib);;
ltake (20, lfib);;

type 'a lbt = LEmpty | LNode of 'a * (unit -> 'a lbt) * (unit -> 'a lbt);;

let lbreadth tree = 
  let rec search queue = 
    match queue with 
    | [] -> LNil 
    | LEmpty::t -> search t 
    | LNode(x, l, r)::t -> LCons(x, lazy (search (t@[l(); r()])))
  in search [tree];;

let tree2 = LNode(1, (fun () -> LNode(2, (fun () -> LEmpty), (fun () -> LEmpty))), 
fun () -> LNode(3, (fun () -> LNode(4, (fun () -> LEmpty), (fun () -> LEmpty))), fun () -> LEmpty));;

ltake (6, lbreadth tree2)

let rec ltree n = LNode(n, 
(fun () -> ltree (2 * n)), 
(fun () -> ltree (2 * n + 1)));;

ltake (20, (lbreadth(ltree 1)));;