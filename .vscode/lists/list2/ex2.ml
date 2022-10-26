let rec fib n =
  if n < 0 then failwith "Negative argument"
  else if n <= 1 then n
  else fib(n - 1) + fib(n - 2);;

let rec fib n = 
  match n with 
  | x when x < 0 -> failwith "Negative argument"
  | x when x <= 1 -> x
  | x -> fib(x - 1) + fib(x - 2);;

  

(*0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89*)

let fibTail n =
  let rec fibInner (n, acc1, acc2) =
    if n < 0 then failwith "Negative Argument"
    else if n = 0 then acc1
    else fibInner(n - 1, acc2, acc1 + acc2)
  in fibInner(n, 0, 1);;

  let fibTail2 n =
    let rec fibInner (x, acc1, acc2) =
      if x < 0 then failwith "Negative Argument"
      else if x = n then acc1
      else fibInner(x + 1, acc2, acc1 + acc2)
    in fibInner(0, 0, 1);;
  
fibTail2 0;;
fibTail2 1;;
fibTail2 2;;
fibTail2 3;;
fibTail2 4;;
fibTail2 5;;
fibTail2 6;;
fibTail2 7;;  

fibTail 0;;
fibTail 1;;
fibTail 2;;
fibTail 3;;
fibTail 4;;
fibTail 5;;
fibTail 6;;
fibTail 7;;

fibTail 0;;
fibTail 1;;
fibTail 2;;
fibTail 3;;
fibTail 4;;
fibTail 5;;
fibTail 6;;
fibTail 7;;

fib 42;;
(*fibTail 42;;*)

