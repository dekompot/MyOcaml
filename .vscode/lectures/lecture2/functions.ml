(function x -> x + x) 6;;

let double = function x -> x + x;;

double 4;;

(*Shortcut notation*)
let double x = x + x;;

double 5;;

(*-> binds to the right*)
let f = function y -> function x -> x * x + y;;

f 2 5;;

let f2 = f 2;;
f2 5;;

let f5 = function z -> f z 5;;
f5 2;;

(*How syntatic sugar works in Ocaml*)
let plus = function x -> function y -> x + y;;
plus 2 3;;

let plus' x = function y -> x + y;;
plus' 2 3;;

let plus'' x y = x + y;;
plus'' 2 3;;

let plus3 = fun x y -> x + y;;
plus3 2 3;;

let id x = x;;

id 5;;

id (3 + 4, "seven");;

id id "OK";;

id ;;

(*uncurried - zwinięta*)

let plus (x, y) = x + y;;

plus(4, 5);;

(*curried - rozwinięta*)
let add x y = x + y;;
add 4 5 ;; 

print_string "hello world";;

