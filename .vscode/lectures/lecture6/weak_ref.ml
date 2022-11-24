(*
hypothetical situation
let x = ref []     'a list ref = {contents=[]}   
x := 1 :: !x
x := true :: !x
*)

let x = ref [];;

x := 1 :: !x;;

let f a b = a;;


(*_weak3 -> int = <fun>*)
let g = f 1;;

g "Hello";;

g;;

g 2;;

let g1 x = f 1 x;;

g1 "Hello";;

g1 2;;