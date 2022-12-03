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