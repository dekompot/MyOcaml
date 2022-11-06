(*1*)
let mirror4 (x, y, z, v) = (y, x, v, z);;

(*tests*)
mirror4 (1, 2, 3, 4);;
mirror4 (1, "Ocaml", 'a', 4.5);;

(*2*)
let rec remove (l, i) = 
  if l = [] then []
  else if i = 0 then List.tl l
  else (List.hd l) :: remove(List.tl l, i - 1);;

(*valid arguments*)
remove ([1; 2; 3; 4; 5], 0);;
remove (['a'; 'b'; 'c'; 'd'; 'e'], 4);;
remove ([1.; 2.; 3.; 4.; 5.], 2);;

(*1-element list*)
remove(["Ocaml"], 0);;
remove(["Ocaml"], 1);;
remove(["Ocaml"], -1);;

(*invalid arguments*)
remove ([1; 2; 3; 4; 5], -1);;
remove (['a'; 'b'; 'c'; 'd'; 'e'], 5);;

(*empty list*)
remove ([], 0);;
remove ([], -1);;
remove ([], 4);;

(*3*)
let rec rotations l =
  if l = [] then failwith "list is empty"
  else if List.tl l = [] then List.hd l
  else if (List.tl (List.tl l)) = [] then (-1. *. (List.hd l)) /. (List.hd (List.tl l))
  else rotations((-1. *. (List.hd l)) :: (List.tl (List.tl l)));;

(*even length list*) 
rotations([80.; 20.; 40.; 20.]);;
rotations([10.; 20.; 5.; 40.]);;
(*uneven length list*) 
rotations([40.; 20.; 10.]);;
rotations([10.; 20.; 40.]);;

(*1-element list*) 
rotations([10.]);;
(*empty list*)
rotations([]);;

