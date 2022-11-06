(*adding an element to the front: O(1)
   , cons operator ::*)
let p (xs, n) = 
  match xs with
  | h::t when n = 0 -> t 
  | _ -> [];;
 
let rec replaceNth (xs, n, x) = 
  match xs with 
  | h::t when n = 0 -> x::t 
  | h::t -> h::replaceNth(t, n - 1, x)
  | [] -> [];;

replaceNth ([1; 2; 3; 4; 5], 0, 0);;
replaceNth ([1; 2; 3; 4; 5], 2, 0);;
replaceNth ([1; 2; 3; 4; 5], 4, 0);;
replaceNth ([1; 2; 3; 4; 5], -1, 0);;
replaceNth ([1; 2; 3; 4; 5], 5, 0);;

replaceNth (['a'], 0, 'b');;
replaceNth (['a'], -1, 'b');;
replaceNth (['a'], 1, 'b');;

replaceNth ([], 0, -2);;

(*Test memory layout*)
let xs = ['a'; 'b'; 'c'; 'd'; 'e'];;

let xs' = replaceNth(xs, 2, 'c');;

xs = xs';;
(*This compares the whole list*)
xs == xs';;
(List.tl xs) == (List.tl xs');;
(List.tl (List.tl xs)) == (List.tl (List.tl xs'));;
(List.tl (List.tl (List.tl xs))) == (List.tl (List.tl (List.tl xs')));;
(List.tl (List.tl (List.tl (List.tl xs)))) == (List.tl (List.tl (List.tl (List.tl xs'))));;

(*For non-mutable types, the behaviour of == guarantees that
   e1 == e2 => compare e1 e2 = 0*)
(List.hd xs) == (List.hd xs');;

let ($.) f g x = f (g x);;

let fourth xs = (List.tl $. List.tl $. List.tl) xs;;

let ys = fourth xs;;
let ys' = fourth xs';;

ys == ys';;
