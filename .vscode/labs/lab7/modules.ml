type ordering = EQ | LT | GT

module type COMPARATOR = 
sig
type t 
val compare : t -> t -> ordering 
end;;

module IntComparator : COMPARATOR with type t = int = 
struct 
  type t = int 
  let compare i j = if i < j then LT else if i > j then GT 
                    else EQ
end;; 

IntComparator.compare 1 2;;
IntComparator.compare 5 3;;
IntComparator.compare 4 4;;

module StringComparator : COMPARATOR with type t = string = 
struct 
  type t = string 
  let compare s1 s2 = if s1 < s2 then LT else if s1 > s2 then GT else EQ
end;;

StringComparator.compare "a" "b";;
StringComparator.compare "aaa" "a";;


module ReverseComparator (C: COMPARATOR) = 
struct 
  type t = C.t 
  let compare e1 e2 = 
    match C.compare e1 e2 with 
    | LT -> GT 
    | GT -> LT 
    | EQ -> EQ;;
end;;

module ReverseIntComparator = ReverseComparator(IntComparator);;

ReverseIntComparator.compare 1 2;;
ReverseIntComparator.compare 5 4;;
ReverseIntComparator.compare 3 3;;

                    
module PriorityQueueImpl (C : COMPARATOR) = 
struct
  type t = C.t 
  type queue = {mutable n : int; mutable arr : C.t option array}
  exception Empty of string

  let capacity = 4
  let create () = {n=0; arr=Array.make capacity None};;

  let isEmpty q = q.n = 0;;

  let increase q = q.arr <- Array.append q.arr (Array.make capacity None)

  let compare q i j = 
    let Some(key_i) = q.arr.(i) and Some(key_j) = q.arr.(j) in C.compare key_i key_j;;

  let swap q i j = 
    let temp = q.arr.(i) in q.arr.(i) <- q.arr.(j); q.arr.(j) <- temp;;

  let left_child parent = Int.shift_left parent 1 + 1;;

  let parent child = Int.shift_right (child - 1) 1;;

  let sink q i = 
    let p = ref i and c = ref (left_child i) and r_c = ref 0 in 
    while !c < q.n do
      r_c := succ !c; 
      if !r_c < q.n && compare q !r_c !c = GT then c := !r_c;
      if compare q !p !c = LT then (swap q !p !c; p := !c; c := left_child !p;)
      else c := q.n done;;

  let swim q i = 
    let c = ref i and p = ref (parent i) in 
    while !c > 0 do 
      if compare q !c !p = GT then (swap q !c !p; c := !p; p := parent !c;)
      else c:= 0 done;;

  let insert q key = if Array.length q.arr = q.n then increase q;
                      q.arr.(q.n) <- Some key; swim q q.n; q.n <- succ q.n;;

  let retrieve q = if isEmpty q then raise (Empty "Empty queue")
                   else let Some(max_key) = q.arr.(0) in swap q 0 (q.n - 1); q.n <- q.n - 1; sink q 0; max_key;;

  let peek q = if isEmpty q then raise (Empty "Empty queue")
               else let Some(key) = q.arr.(0) in key;;

  let print_entries q print = 
    for i = 0 to q.n - 1 do let Some(key) = q.arr.(i) in print key; print_string " " done; 
    print_newline();;

end

module StringMaxQueue = PriorityQueueImpl(StringComparator);;

let queue = StringMaxQueue.create();;

StringMaxQueue.isEmpty queue;;
StringMaxQueue.insert queue "a";;
StringMaxQueue.peek queue;;
StringMaxQueue.print_entries queue print_string ;;
StringMaxQueue.insert queue "b";;
StringMaxQueue.peek queue;;
StringMaxQueue.print_entries queue print_string ;;
StringMaxQueue.insert queue "c";;
StringMaxQueue.insert queue "d";;
StringMaxQueue.insert queue "e";;
StringMaxQueue.peek queue;;
StringMaxQueue.print_entries queue print_string ;;
StringMaxQueue.retrieve queue;;
StringMaxQueue.peek queue;;
StringMaxQueue.print_entries queue print_string ;;
StringMaxQueue.retrieve queue;;
StringMaxQueue.peek queue;;
StringMaxQueue.retrieve queue;;
StringMaxQueue.peek queue;;
StringMaxQueue.print_entries queue print_string ;;
StringMaxQueue.retrieve queue;;
StringMaxQueue.peek queue;;
StringMaxQueue.retrieve queue;;
StringMaxQueue.print_entries queue print_string ;;


module IntMaxQueue = PriorityQueueImpl(IntComparator);;

let queue = IntMaxQueue.create();;

IntMaxQueue.isEmpty queue;;
IntMaxQueue.insert queue 10;;
IntMaxQueue.insert queue 7;;
IntMaxQueue.insert queue 20;;
IntMaxQueue.insert queue 14;;
IntMaxQueue.insert queue 22;;
IntMaxQueue.insert queue 16;;
IntMaxQueue.print_entries queue print_int;;
IntMaxQueue.retrieve queue;;
IntMaxQueue.retrieve queue;;
IntMaxQueue.print_entries queue print_int;;
IntMaxQueue.insert queue 32;;
IntMaxQueue.print_entries queue print_int;;

module IntMinQueue = PriorityQueueImpl(ReverseIntComparator);;

let queue = IntMinQueue.create();;

IntMinQueue.isEmpty queue;;
IntMinQueue.insert queue 10;;
IntMinQueue.insert queue 7;;
IntMinQueue.insert queue 20;;
IntMinQueue.insert queue 14;;
IntMinQueue.insert queue 22;;
IntMinQueue.insert queue 16;;
IntMinQueue.print_entries queue print_int;;
IntMinQueue.retrieve queue;;
IntMinQueue.retrieve queue;;
IntMinQueue.print_entries queue print_int;;
IntMinQueue.insert queue 4;;
IntMinQueue.print_entries queue print_int;;

module type PriorityQueue = 
sig 
  type t 
  type queue 
  val create : unit -> queue 
  val insert : queue -> t -> unit 
  val retrieve : queue -> t 
  val isEmpty : queue -> bool
  val peek : queue -> t
end

module MinIntInterfacePQ : PriorityQueue = IntMinQueue;;
module MaxIntInterfacePQ : PriorityQueue = IntMaxQueue;;
module StringInterfacePQ : PriorityQueue = StringMaxQueue;;
