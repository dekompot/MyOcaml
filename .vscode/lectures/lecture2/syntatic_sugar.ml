(*Syntatic sugar
   All functions are still 1-argument*)
let plus = function x -> function y -> x + y;;

plus 2 3;;

let plus' x = function y -> x + y;;

plus' 2 3;;

let plus'' x y = x + y;;

plus'' 2 3;;

let plus3 = fun x y -> x + y;;

plus3 2 3;;