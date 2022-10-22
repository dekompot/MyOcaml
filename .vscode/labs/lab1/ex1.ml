let mirror4 (x, y, z, v) = (y, x, v, z);;

(*tests*)
mirror4 (1, 2, 3, 4);;
mirror4 (1, "Ocaml", 'a', 4.5);;