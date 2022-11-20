let x = lazy (true||false, 3 * 4);;

Lazy.force x;;

x;;

let f x = "OK";;

(*we never use this argument*)
f(lazy(1 / 0));;
f((1 / 0));;