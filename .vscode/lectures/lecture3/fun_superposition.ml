let ($.) f g = fun x -> f (g x);;

let third xs3 = (List.hd $. List.tl $. List.tl) xs3;;

third [1; 2; 3; 4; 5];;

let next_char = Char.chr $. (+) 1 $. Char.code;;

next_char 'b';;

abs 1 - 5;;
(*@@ binds to the right*)
abs @@ 1 - 5;;

let div x y = x /. y;;
let plus x y = x + y;;

div 1. 2. +. 3.;;
div 1. @@ 2. +. 3.;;

(*reverse-application operator*)
5 - 12 |> abs |> succ;;
(*the same as*)
succ (abs (5 - 12));;