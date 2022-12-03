module type QUEUE_FUN = 
sig 
  type 'a t
  exception Empty of string 
  val empty : unit -> 'a t
  val enqueue : 'a * 'a t -> 'a t
  val first : 'a t -> 'a 
  val dequeue : 'a t -> 'a t 
  val isEmpty : 'a t -> bool 


end;;

module Queue : QUEUE_FUN = 
struct 
  type 'a t = 'a list 
  exception Empty of string
  let empty() = []
  let enqueue (e, q) = q@[e]
  let dequeue q = 
    match q with 
    | _::t -> t
    | [] -> []
  
  let first q = 
    match q with 
    | h::_ -> h
    | [] -> raise (Empty "Empty queue")

  let isEmpty q = q = []  

end;;

let queue = ref (Queue.empty());;

let (<|) q e = Queue.enqueue (e, q);;

queue := !queue <| 1 <| 2 <| 3 <| 4;;

Queue.first !queue;;
queue := Queue.dequeue !queue;;
Queue.first !queue;;
queue := Queue.dequeue !queue;;
Queue.first !queue;;
queue := Queue.dequeue !queue;;
Queue.first !queue;;
queue := Queue.dequeue !queue;;
queue := Queue.dequeue !queue;;

module type PRINTABLE = 
sig 
  type 'a t 
  val (!<) : ('a -> string) -> 'a t -> string
end;;

module Queue2List : QUEUE_FUN with type 'a t = 'a list * 'a list = 
struct
  type 'a t = 'a list * 'a list
  exception Empty of string
  let empty() = ([], [])
  let enqueue (e, (q1, q2)) = 
    match q1 with 
    | [] -> ([e], [])
    | _ -> (q1, e::q2)
  
  let dequeue q = 
    match q with 
    | (_::[], q2) -> (List.rev q2, [])
    | (_::tl, q2) -> (tl, q2)
    | ([], []) -> ([], [])
  
  let first (q1, _) = 
    match q1 with 
    | [] -> raise (Empty "Error: empty queue")
    | h::tl -> h

  let isEmpty (q1, q2) = q1 = []
end;;

let print_list l to_str = (List.fold_left (fun acc v -> acc^" "^(to_str v)) "" l);;

let print_queue to_str qq  = "[" ^ print_list (fst qq) to_str ^ "], [" ^ print_list (snd qq) to_str ^ "]";;
let (!<) qq = print_queue (fun x -> x) qq;;

let q = ref (Queue2List.empty());;

let (<|) qq e = Queue2List.enqueue (e, qq);;

q := !q <| "1" ;; 
!< (!q);;

q := !q <| "2" <| "3" <| "4";;
(*[1], [4, 3, 2]*)
!< (!q);;

Queue2List.first !q;;

q := Queue2List.dequeue !q;;
(*[2, 3, 4], []*)
!< !q;;
Queue2List.first !q;;

q := !q <| "5";;
!< !q;;
Queue2List.first !q;;

q := Queue2List.dequeue !q;;
Queue2List.first !q;;
q := Queue2List.dequeue !q;;
Queue2List.first !q;;
q := Queue2List.dequeue !q;;
Queue2List.first !q;;
!< !q;;
Queue2List.first !q;;

q := Queue2List.dequeue !q;;
!< !q;;
Queue2List.first !q;;