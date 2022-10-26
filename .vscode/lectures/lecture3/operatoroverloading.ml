let (=>) b1 b2 = 
  match (b1, b2) with 
  | (true, false) -> false 
  | _ -> true;;

false => true;;
true => false;;

(+);;
(=);;

(*overload infix operator*)
let (@) n1 n2 =
  (n1 * 10) + n2;;

1 @ 3;;

(*overload postfix operator*)
let (!*) a = 1. /. a;;

!* 2.0;;

(*infix operator can be converted to 
   2-argument curried function*)
let pred = (+) (-1) ;;

pred 3;;

let (++) (x1, y1)(x2, y2) = (x1 +. x2, y1 +. y2);;

let c = (2.5, 3.5) in c ++ c;;

(*raises each element in a list to the power
   of x*)
let ( ** ) l x = List.map (fun k -> Float.pow (float(k)) (float(x))) l;;

[1; 2; 3] ** 4;;

let (!.) l = List.map (fun x -> (-x)) l;;

!. [1; 2; -3];;

let (=) l x = List.filter (fun y -> y = x) l;;

[1; 2; 3; 3; 2] = 2;;

