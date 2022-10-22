let rec flatten1 xss = 
  if xss = [] then []
  else (List.hd xss)@(flatten1(List.tl xss));;

flatten1 [[1; 2]; [3; 4; 5]];;
flatten1 [['a'; 'b']; ['c'; 'd']; ['e'; 'f']];;
flatten1 [[1.; 2.04]; [3.4; 5.6; 6.7]];;
flatten1 [[]; []];;
flatten1 [];;

let rec flatten1' xss =
  match xss with 
  | [] -> []
  | h::t -> h@(flatten1(t));;

flatten1' [[1; 2]; [3; 4; 5]];;
flatten1' [['a'; 'b']; ['c'; 'd']; ['e'; 'f']];;
flatten1' [[1.; 2.04]; [3.4; 5.6; 6.7]];;
flatten1' [[]; []];;
flatten1' [];;
  