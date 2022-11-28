module type STACK = 
sig 
  type 'a t 
  exception Empty of string
  val create: unit -> 'a t
  val push: 'a * 'a t -> 'a t
  val top: 'a t -> 'a 
  val pop: 'a t -> 'a t
  val isEmpty: 'a t -> bool
end;;

module Stack : STACK = 
struct 
  type 'a t = EmptyStack | Push of 'a * 'a t
  exception Empty of string
  let create () = EmptyStack
  let push(e, s) = Push(e, s)
  let top = function | Push(e, _) -> e | EmptyStack -> raise (Empty "empty stack")
  let pop = function | Push(_, s) -> s | EmptyStack -> EmptyStack
  let isEmpty s = s = EmptyStack
end;;

let s = ref (Stack.push(1, Stack.push(2, Stack.create())));;

Stack.top(!s);;
s := Stack.pop(!s);;
Stack.top(!s);;
s := Stack.pop(!s);;
Stack.top(!s);;