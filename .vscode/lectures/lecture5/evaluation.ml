(*alternative or conjunction is evaluated
   lazily*)
true || failwith "error";;

false && failwith "error";;

(*to control the moment of evaluation, 
   we can pass in a value in form of a
   function*)
function () -> (1.0 /. 0.0);;

(function () -> (1.0 /. 0.0))();;

let f = function () -> 2 + 5;;
(*with syntactic sugar*)
let f' () = 2 + 5;;

f();;
f'();;

let rec factorial n = 
  if n <= 0 then 1 else n * factorial(n - 1);;

let my_if (b, t, f) = 
  if b then t else f;;

  (*does not terminate*)
let rec factorial2 n = 
  my_if (n <= 0, 1, factorial2 (n - 1));;

(*factorial2(4);;*)

let rec if_funs (b, t, f) = 
  if b then t() else f();;

let rec factorial3 n = 
  if_funs (n <= 0, (fun () -> 1), (fun () -> n * factorial3 (n - 1)));;

factorial3 (4);;

(*how does it work with partial application?*)
let f a b = if a < b then failwith "error" else 0;;

f 1 2;;