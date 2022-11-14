exception Failure' of string;;
exception Invalid_argument' of string;;
exception Not_found';;

let failwith' s = raise (Failure' s);;

let invalid_arg' s = raise (Invalid_argument' s);;

let hd = function 
  [] -> failwith' "hd"
  | h::t -> h;;

hd [];;
hd [1];;

let rec nth l n = 
  match l with 
  [] -> failwith "nth"
  | a::l -> 
    if n = 0 then a 
    else 
      if n > 0 then nth l (n - 1)
      else invalid_arg' "List.nth";;

nth [1; 2; 3] (-1);;
nth [1; 2; 3] 3;;

let rec assoc x = function 
  [] -> raise Not_found'
  | (a, b)::l -> if a = x then b else assoc x l;;

assoc 2 [(1, 'a'); (2, 'b')];;
assoc 3 [(1, 'a'); (2, 'b')];;

let find key dict = 
  try List.assoc key dict with Not_found -> "failure";;

"Char " ^ find 2 [(1, "a"); (2, "b")];;
"Char " ^ find 3 [(1, "a"); (2, "b")];;