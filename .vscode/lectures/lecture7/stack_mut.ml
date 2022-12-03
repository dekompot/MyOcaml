(*sig - end defines what is accessible from the outside*)
module type STACK_MUT = 
sig 
  type 'a t
  val create: unit -> 'a t
  val push: 'a * 'a t -> unit
  val top: 'a t -> 'a 
  val pop: 'a t -> unit
  val isEmpty: 'a t -> bool 
end;;

module StackMutList = 
struct 
  type 'a t = {mutable l : 'a list}
  exception Empty of string 
  let create() = {l = []}
  let push(e, s) = s.l <- e::s.l
  let top s = 
    match s.l with 
    | h::_ -> h
    | [] -> raise (Empty "")
  let pop s = 
    match s.l with 
    | _::t -> s.l <- t 
    | [] -> ()

  let isEmpty s = s.l = []
end;;

let s = StackMutList.create();;
StackMutList.push(1, s);;
StackMutList.push(2, s);;
StackMutList.top s;;
StackMutList.pop s;;
StackMutList.pop s;;
StackMutList.top s;;

module StackMutAr = 
struct 
  type 'a t = {mutable n: int; mutable a : 'a option array; }
  exception Empty of string 
  let size = 5
  let create() = {n=0; a = Array.make size None}
  let increase s = s.a <- Array.append s.a (Array.make size None)
  let push(e, s) = begin if s.n = Array.length s.a then increase s;
                            s.a.(s.n) <- Some e ;
                            s.n <- succ s.n;
  end
                    
  let top s = if s.n = 0 then raise (Empty "")
              else match s.a.(s.n - 1) with
              | Some e -> e
              | None -> failwith "???"
  let pop s = if s.n = 0 then () else s.n <- s.n - 1 
  let isEmpty s = s.n = 0
end;;

let s = StackMutAr.create();;

StackMutAr.push(1, s);;
StackMutAr.push(2, s);;
StackMutAr.top s;;
StackMutAr.pop s;;
StackMutAr.top s;;
StackMutAr.pop s;;
StackMutAr.top s;;

module SML : STACK_MUT = StackMutList;;
module SMA : STACK_MUT = StackMutAr;;

let sl = SML.create();;
let sa = SMA.create();;

(*We can't mix between modules
this and similar won't work
SMA.isEmpty sl;;  
*)
