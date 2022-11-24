(*we have two types of equalities: structural (=, <>) and physical (==, !=)*)

let x = ref 0;;

let y = ref 0;;

x = y;;

x == y;;

[1; 2; 3] = [1; 2; 3];;
[1; 2; 3] == [1; 2; 3];;

"OK" = "OK";;
"OK" == "OK";;

let id = fun x -> x;;

(*throws an exception because functions can't be compared structurally*)
id = id;;
id == id;;

1.2 == 1.2;;
1.2 = 1.2;;
1 == 1;;
'a' == 'a';;