module Pair = functor (El : sig type t end) -> 
                        struct type pair = El.t * El.t end;;

module Pair2 (El : sig type t end) = struct type pair = El.t * El.t end;;

