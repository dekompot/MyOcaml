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

module TupleComparator : COMPARATOR with type t = int * int = 
struct
  type t = int * int 
  let compare t1 t2 = 
    match IntComparator.compare (fst t1) (fst t2) with
    | EQ -> IntComparator.compare (snd t1) (snd t2)
    | result -> result 
end;;

TupleComparator.compare (2, 3) (1, 4);;
TupleComparator.compare (2, 3) (2, 4);;
TupleComparator.compare (1, 1) (1, 1);;
                    
module PriorityQueueImpl (C : COMPARATOR) = 
struct
  type queue = {mutable n : int; mutable size : int; mutable arr : C.t option array}
  exception Empty of string 
  exception Full of string 
  
  let capacity = 5;;

  let create () = {n=0; size=capacity; arr=Array.make capacity None};;

  let compare q i j = 
    let Some(key_i) = q.arr.(i) and Some(key_j) = q.arr.(j) in C.compare key_i key_j;;

  let swap q i j = 
    let temp = q.arr.(i) in q.arr.(i) <- q.arr.(j); q.arr.(j) <- temp;;

  let left_child parent = Int.shift_left parent 1 + 1;;

  let parent child = Int.shift_right (child - 1) 1;;

  let sink q i = 
    let p = ref i and c = ref (left_child i) in 
    while !c < q.n do
      if succ !c < q.n && compare q (succ !c) !c = GT then c := succ !c;
      if compare q !p !c = LT then (swap q !p !c; p := !c; c := left_child !p;)
      else c := q.n done;;

  let swim q i = 
    let c = ref i and p = ref (parent i) in 
    while !c > 0 do 
      if compare q !c !p = GT then (swap q !c !p; c := !p; p := parent !c;)
      else c:= 0 done;;

  let insert q key = 
    match q.n with 
    | i when i = q.size -> raise (Full "Full queue")
    | i -> begin (q.arr.(i) <- Some key; swim q i; q.n <- succ q.n) end;;

  let retrieve q = 
    match q.n with 
    | 0 -> raise (Empty "Empty queue")
    | n -> let Some(max_key) = q.arr.(0) in swap q 0 (n - 1); q.n <- q.n - 1; sink q 0; max_key;;

  let peek q = 
    match q.n with 
    | 0 -> raise (Empty "Empty queue")
    | _ -> let Some(key) = q.arr.(0) in key;;

  let print_entries q print = 
    for i = 0 to q.n - 1 do let Some(key) = q.arr.(i) in print key; print_string " " done; 
    print_newline();;

end

module StringMaxQueue = PriorityQueueImpl(StringComparator);;

let queue = StringMaxQueue.create();;

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

IntMaxQueue.insert queue 10;;


