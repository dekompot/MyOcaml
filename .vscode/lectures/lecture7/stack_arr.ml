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