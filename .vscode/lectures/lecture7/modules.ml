module M = (
  struct 
    type buffer = int ref 
    let create() = ref 0 
    let add x = incr x 
    let get x = if !x>0 then (decr x; 1) else failwith "Empty"
  end
  : sig 
    type buffer 
    val create : unit -> buffer 
    val add : buffer -> unit 
    val get : buffer -> int 
  end 
);;

module type PRODUCER = 
sig 
  type buffer 
  val create : unit -> buffer 
  val add : buffer -> unit  
end;;

module type CONSUMER = 
sig 
  type buffer 
  val get : buffer -> int 
end;;

module Producer = (M:PRODUCER);;
module Consumer = (M:CONSUMER);;

(*they are not compatible
let buf = Producer.create() in Producer.add buf ; Consumer.get buf;;*)

module Producer' = (M:PRODUCER with type buffer = M.buffer);;
module Consumer' = (M:CONSUMER with type buffer = M.buffer);;

let buf = Producer'.create();;

Producer'.add buf; Producer'.add buf;;

Consumer'.get buf;;
Consumer'.get buf;; 
Consumer'.get buf;;