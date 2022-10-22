let x1 = 5;;

let x1 = 4;;

let x1 = 7.5;;

x1 +. 6.7;;

x1 +. float_of_int 6;;

float_of_int 5;;

let napis1 = "Ala";;
let napis2 = "ma";;
let napis3 = "kota";;

napis1 ^ " " ^ napis2 ^ " " ^ napis3;;

(let z = x1 +. x1 and y = 2.0 in 
  z +. z +. (let z = 10.0 in z +. y)) +. 1.0;;

let k3 = (3 + 4, 3.0, 2 < 4);;

(*This is a function!*)
let k3_1 (x, y, z) = x;; 

k3_1 k3;;

(* tuples can be compared *)
k3 = (8 - 1, 3.0, 1 < 5);;

let k2 = (3 + 4, (2., 1 < 4));;

fst k2;;

snd k2;;

fst (snd k2);;

let xs = 1. :: x1 :: 2.5 :: [];;

xs = [1.; 7.5; 2. +. 0.5]

let head = (List.hd xs);;

head = 1.;;

(*Lists consist of head and tail (the rest of elements)*)
List.tl xs;;

let multi_dim_list = [[1; 2]; [3; 4]; [5; 6]];;

List.length xs;;

xs@[9.; 10.];;

List.rev [1; 2; 3]

let xss = [[4.; 5.]; xs; 1. :: 2. :: []];;

List.length xss;;