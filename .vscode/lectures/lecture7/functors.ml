module Pair = functor (El : sig type t end) -> 
                        struct type pair = El.t * El.t end;;

module Pair2 (El : sig type t end) = struct type pair = El.t * El.t end;;

module type X = sig 
  val x : int 
end ;;
  
module IncX(M: X) = struct
let x = M.x + 1
end;;
  
module A = struct let x = 0 end;;
  
module B = IncX(A);;
  
print_int(B.x);;
  
module AddX (M: X) = struct 
  let add y = M.x + y 
end;;
  
module Add42 = AddX(struct let x = 42 end);;
  
print_int(Add42.add 1);;

(*functors shouldn't be confused with extends keyword 
   because the resulting interface is totally different
Add42.x;;*)
  