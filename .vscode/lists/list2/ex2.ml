let rec fib n =
  if n == 0 then 0
  else if n == 1 then 1
  else fib(n - 1) + fib(n - 2);;

(*0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89*)
fib 0;;
fib 1;;
fib 2;;
fib 3;;
fib 4;;
fib 5;;
fib 6;;
fib 7;;

let fibTail n =
  let rec fibInner (n, acc1, acc2) =
    if n = 0 then acc1
    else if n = 1 then acc2
    else fibInner(n - 1, acc2, acc1 + acc2)
  in fibInner(n, 0, 1);;

fibTail 0;;
fibTail 1;;
fibTail 2;;
fibTail 3;;
fibTail 4;;
fibTail 5;;
fibTail 6;;
fibTail 7;;