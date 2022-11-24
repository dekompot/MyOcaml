(*constructor: 'a -> 'a ref*)
let x = ref 5;;

(*deallocator: 'a ref -> 'a*)
!x;;

(*dereference: 'a ref -> 'a -> unit*)
x := 7;;

!x;;

let x = ref 5;; 

let y = x;;

y := 8;;

!x;;

let z = ref 0;;

z := 5;;

let v = z := !z + 2;;