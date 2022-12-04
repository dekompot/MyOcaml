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
  type 'a t = {mutable f: int; mutable r: int; mutable arr: 'a option array}
  exception Empty of string
  exception Full of string

  let succ i q = succ(i) mod (Array.length q.arr)
  let isEmpty q = q.r = q.f
  let isFull q = succ (q.r) q = q.f
  let empty size = {f=0; r=0; arr=Array.make (size + 1) None}
  let enqueue (e, q) = 
  if isFull q then failwith "full queue"
  else q.arr.(q.r) <- Some(e);
  q.r <- succ (q.r) q;;
  let dequeue q = if not (isEmpty q) then q.f <- succ (q.f) q

  let first q = if isEmpty q then raise (Empty "empty queue")
                else let Some(e) = q.arr.(q.f) in e

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
