type ('a, 'b) bt = 
| Leaf of 'a 
| Node of 'b * ('a, 'b) bt * ('a, 'b) bt;;

let rec nOfLeaves = function 
  | Leaf _ -> 1 
  | Node(_, tL, tR) -> nOfLeaves tL + nOfLeaves tR;;

let tree = Node('*', Node('+', Leaf 2, Leaf 3), 
Node('/', Leaf 9, Leaf 3));;

nOfLeaves tree;;