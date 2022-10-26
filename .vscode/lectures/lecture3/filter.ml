let rec filter pred xs = 
  match xs with 
  | [] -> []
  | h::t -> if pred h then h::filter pred t
  else filter pred t;;

filter (fun x -> x mod 2 = 0) [1; 2; 3; 4; 5];;

List.filter (fun s -> String.length s <= 6) ["Litwo"; "Ojczyzno"; "Moja"];;

(*function syntax works like match, but we don't have to pass matching
   argument as an argument to the function*)
(*does not compile??*)
(*it's complexity is O(n^2)*)
let filter_bad p = 
  let rec find acc = function (*instead of match*)
  | [] -> acc 
  | x :: xs -> if p x then find (acc @ [x]) xs else find acc xs
  in find [];;

(*???*)
filter_bad (fun x -> x mod 3 = 0) [1; 3; 4; 6; 7; 9];;

(*This has O(n) complexity*)
let filter2 p =
  let rec find acc = function 
  | [] -> List.rev acc 
  | h::t -> if p h then find (h::acc) t else find acc t
in find [];;

let primes to_n = 
  let rec sieve n =
    if n <= to_n then n::sieve(n + 1) else []
  in let rec find_primes xs = 
    match xs with 
    | [] -> [] 
    | h::t -> h::find_primes (List.filter (fun n -> n mod h <> 0) t)
  in find_primes (sieve 2);;

  primes 10;;

  primes 30;;