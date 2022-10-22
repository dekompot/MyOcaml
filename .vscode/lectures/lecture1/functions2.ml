let rec flatten1 xss = 
  if xss = [] then []
  else (List.hd xss)@(flatten1(List.tl xss));;

flatten1 [[1; 2]; [3; 4; 5]];;
flatten1 [['a'; 'b']; ['c'; 'd']; ['e'; 'f']];;
flatten1 [[1.; 2.04]; [3.4; 5.6; 6.7]];;
flatten1 [[]; []];;
flatten1 [];;

let rec flatten1' xss =
  match xss with 
  | [] -> []
  | h::t -> h@(flatten1(t));;

flatten1' [[1; 2]; [3; 4; 5]];;
flatten1' [['a'; 'b']; ['c'; 'd']; ['e'; 'f']];;
flatten1' [[1.; 2.04]; [3.4; 5.6; 6.7]];;
flatten1' [[]; []];;
flatten1' [];;
  

let rec add_1 xs = 
  if xs = [] then []
  else List.hd xs + 1 :: add_1(List.tl xs);;

let rec double l = 
  if l = [] then []
  else (2 * (List.hd l)) :: double(List.tl l);;
  
let l = [1; 2; 3];;
double l;;
l;;

let rec count (x, l) = 
  if l = [] then 0
  else if List.hd l = x then 1 + count(x, List.tl l)
  else count(x, List.tl l);;

count('a', ['a'; 'c'; 'a']);;
count('a', ['c'; 'd'; 'e']);;
count([1], [[1]; [1; 3]; [1]]);;
count(1, []);;

let rec count' (x, l) = 
  match l with 
  | [] -> 0
  | h::t when h = x -> 1 + count'(x, t)
  | _::t -> count'(x, t);;

count'('a', ['a'; 'c'; 'a']);;
count'('a', ['c'; 'd'; 'e']);;
count'([1], [[1]; [1; 3]; [1]]);;
count'(1, []);;

(*
let rec replicate (x, n) = 
  if n < 0 then failwith ("negative argument" ^ (string_of_int n))
  else if n = 0 then []
  else x :: replicate(x, n - 1);;*)

  
let rec replicate (x, n) = 
  if n <= 0 then []
  else x :: replicate(x, n - 1);;

replicate('a', 3);;
replicate('a', 0);;
replicate('a', -3);;

let rec sqrList l = 
  if l = [] then []
  else let h = List.hd l in 
  (h * h) :: sqrList(List.tl l);;

sqrList [1; 2; 3; 4];;
sqrList [];;

let rec sqrList l = 
  match l with 
  | [] -> []
  | h::t -> (h * h)::sqrList(t);;

sqrList [1; 2; 3; 4];;
sqrList [];;

let rec palindrome l = 
  l = List.rev l;;

let xs = [1; 2; 3];;

xs = List.rev (List.rev xs);;

palindrome ['a'; 'b'; 'b'; 'a'];;
palindrome ['a'; 'b'; 'c'; 'b'; 'a'];;
palindrome ['a'];;
palindrome [];;
palindrome ['x'; 'y'; 'z'; 'x'];;
palindrome ['x'; 'y'; 'z'; 'z'; 'x'];;

let rec listLength l = 
  if l = [] then 0
  else 1 + listLength(List.tl l);;

listLength [1; 2; 3; 4; 5];;
listLength ['a'];;
listLength [];;
listLength [2; 3];;

let rec listLength' l = 
  match l with 
  | [] -> 0
  | _::t -> 1 + listLength' t;;

listLength' [1; 2; 3; 4; 5];;
listLength' ['a'];;
listLength' [];;
  