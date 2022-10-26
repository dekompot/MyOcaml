(*Inserts to an ordered list based on
   some ordering*)
let rec insert precede elem xs = 
  match xs with 
  | [] -> [elem]
  | h::t as ys -> if precede elem h then elem::ys
  else h::(insert precede elem t);;

(*creates a specialized version*)
let insert_le elem xs = insert (<=) elem xs;;

insert_le 4 [1; 2; 3; 5; 6];;
insert_le 4 [1; 2; 3; 4; 5];;

let insert_ge elem = insert (>=) elem;;

insert_ge 6 [5; 4; 3; 2; 1];;
insert_ge 4 [6; 5; 3; 2; 1];;

