module type QUEUE_MUT = 
sig
  type 'a t
  exception Empty of string
  exception Full of string
  val empty: int -> 'a t
  val enqueue: 'a * 'a t -> unit
  val dequeue : 'a t -> unit 
  val first : 'a t -> 'a 
  val isEmpty : 'a t -> bool 
  val isFull : 'a t -> bool
end;;


module QueueMut : QUEUE_MUT = 
struct
  type 'a t = {n: int; mutable f: int; mutable r: int; mutable arr: 'a array}
  exception Empty of string
  exception Full of string
  let empty size = {n = size + 1; f=0; r=0; arr=[||] }
  let enqueue (e, q) = 
    match (q.f, q.r) with 
  | (f, r) when f = (r + 1) mod q.n -> failwith "full queue"
  (*make n x returns an array of length n, initialized with x
     if x is mutable, it is shared among all entries*)
  | (f, r) when f = r -> begin q.arr <- Array.make q.n e ; q.r <- (q.r + 1) mod q.n ; print_string "case 2\n" end
  | _ -> q.arr.(q.r) <- e; q.r <- (q.r + 1) mod q.n; print_string "case 3\n";;

  let dequeue q = if (q.r <> q.f) then q.f <- (q.f + 1) mod q.n

  let first q = if q.r = q.f then raise (Empty "empty queue")
  else q.arr.(q.f)
  let isEmpty q = q.r = q.f
  let isFull q = (q.r + 1) mod q.n = q.f
end;;

let (<|) q e = QueueMut.enqueue (e, q); q;;
let q = QueueMut.empty 3;;

QueueMut.enqueue (1, q);;
QueueMut.enqueue (2, q);;
QueueMut.enqueue (3, q);;

QueueMut.first q;;
QueueMut.dequeue q;;
QueueMut.first q;;
QueueMut.dequeue q;;
QueueMut.first q;;
QueueMut.dequeue q;;
QueueMut.isEmpty q;;
QueueMut.isFull q;;
QueueMut.first q;;
