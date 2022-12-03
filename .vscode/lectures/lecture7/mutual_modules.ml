module M1 = 
(struct 
  type buffer = int ref 
  (*inner module*)
  module M_hide = 
    struct
      let create() = ref 0
      let add x = incr x 
      let get x = if !x>0 then (decr x; 1) else failwith "Empty"      
    end
    module Producer = M_hide
    module Consumer = M_hide
  end 
    : sig 
      type buffer 
      module Producer : sig
                            val create : unit -> buffer
                            val add : buffer -> unit 
                        end
      module Consumer : sig 
                            val get : buffer -> int 
                        end
    end 
);;

let buf = M1.Producer.create();;

M1.Producer.add buf;;

M1.Consumer.get buf;;
