(*każde koło pokonuje tyle zębów na sekundę, co pierwsze koło,
   ale co drugie porusza się w przeciwną stronę. 
   Mając więc listę (z1, z2, ... zN) chcemy
   obliczyć vN = (-1)^(N-1) * z1 / zN, 
   mając v1 = 1*)

let rec rotations l =
  if l = [] then failwith "list is empty"
    (*wykonuje sie tylko dla listy 1-elementowej*)
  else if List.tl l = [] then List.hd l
    (*jesli lista ma dwa elementy, podziel pierwszy przez drugi*)
  else if (List.tl (List.tl l)) = [] then (-1. *. (List.hd l)) /. (List.hd (List.tl l))
    (*z kazdym wywolaniem usuwamy drugi element*)
  else rotations((-1. *. (List.hd l)) :: (List.tl (List.tl l)));;

(*even length list*) 
rotations([80.; 20.; 40.; 20.]);;
rotations([1.; 20.; 5.; 4.]);;
(*uneven length list*) 
rotations([40.; 20.; 10.]);;
rotations([1.; 20.; 4.]);;
(*1-element list*) 
rotations([10.]);;
(*empty list*)
rotations([]);;


let rec rotations l =
  if l = [] then failwith "list is empty"
    (*wykonuje sie tylko dla listy 1-elementowej*)
  else if List.tl l = [] then float_of_int (List.hd l)
    (*jesli lista ma dwa elementy, podziel pierwszy przez drugi*)
  else if (List.tl (List.tl l)) = [] 
    then (float_of_int (-1 * (List.hd l))) /. (float_of_int (List.hd (List.tl l)))
    (*z kazdym wywolaniem usuwamy drugi element*)
  else rotations((-1 * (List.hd l)) :: (List.tl (List.tl l)));;

(*even length list*) 
rotations([80; 20; 40; 20]);;
rotations([1; 20; 5; 4]);;
(*uneven length list*) 
rotations([40; 20; 10]);;
rotations([1; 20; 4]);;
(*1 element list*) 
rotations([10]);;
(*empty list*)
rotations([]);;