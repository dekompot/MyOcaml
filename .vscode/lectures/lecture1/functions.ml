let double = fun x -> x * 2;;

double 6;;

fun x -> x * 3;;

(fun x -> x * 3) 4;;

let twice x = x * 2;;

(*binds to function*)
twice 2 + 3;;

twice (2 + 3);;

let rec factorial n = 
  if n = 0 then 1 else n * factorial(n - 1);;

factorial(4);;

factorial(-1);;

sqrt (float_of_int 16);;

let rec factorial n = 
  if n = 0 then 1
  else if n > 0 then n * factorial(n - 1)
  else failwith "negative argument";;

factorial(0);;
factorial(4);;
factorial(-4);;

(*Functions take in 1 and only 1 argument! You can pass in multiple parameters as a tuple*)
(fun p -> fst p + snd p)(4, 5);;

let add = fun (x, y) -> x + y;;

add(2, 3);;

(fun (x, y) -> x + y)((4, 5));;

let p = 10;;

let f x = (x, p, x + p);;

f p;;

let p = 1000;;

f p;;

(* Example of static binding
  it means that the global variables
in a function are binded during function
definition
   5 * (2 + 3) = 25*)
let a = 3
  in let f x = x + a
  and a = 5
  in a * f 2;;
  
let id x = x;;
  
id 5;;
id (3 + 4, "seven");;
  
let last xs = 
  if xs = [] then failwith "empty list"
  else List.hd(List.rev xs);;
  
last [1; 2; 3];;
  
last [];;
  
let rec mult_2 xs = 
  if xs = [] then []
  else List.hd xs * 2 :: mult_2(List.tl xs);;
  
mult_2 [1; 2; 3];;