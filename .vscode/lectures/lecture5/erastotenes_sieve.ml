type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec lfrom k = LCons(k, fun () -> lfrom(k + 1));;

let rec ltake (n, lxs) = 
  match (n, lxs) with 
  | (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x, xf)) -> x::ltake(n - 1, xf());;

let rec lfilter pred lxs = 
  match lxs with 
  | LNil -> LNil 
  | LCons(x, xf) -> if pred x then LCons(x, fun () -> lfilter pred (xf())) else lfilter pred (xf());;
    
let primes = 
  let rec sieve = function
  | LCons(p, nf) -> LCons(p, 
      function () -> sieve (lfilter (fun x -> x mod p <> 0) (nf())))
  | LNil -> failwith "Impossible"
in sieve (lfrom 2);;

ltake (5, primes);; 

let rec ltakeWithTail = function 
| (0, lxs) -> ([], lxs)
| (_, LNil) -> ([], LNil)
| (n, LCons(x, xf)) -> let (xs, tail) = ltakeWithTail (n - 1, xf()) in (x::xs, tail);;

let (p1, t) = ltakeWithTail (3, primes);;
let (p2, t) = ltakeWithTail (4, t);;
let (p3, t) = ltakeWithTail (5, t);;