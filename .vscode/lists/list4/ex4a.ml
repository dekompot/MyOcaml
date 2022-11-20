type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let depth_inner tree = 
  let rec compute tree depth = 
    match tree with 
    | Empty -> 0
    | Node(_, l, r) -> depth + compute l (depth + 1)
    + compute r (depth + 1)
  in compute tree 0;;

let tree = Node(1, 
  Node(2, Node(4, Empty, Empty), Empty), 
  Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty));;

depth_inner tree;;

let tree2 = Node(1, Node(2, Node(4, Node(5, Empty, Empty), Empty), Empty), 
Node(3, Empty, Node(7, Empty, Node(8, Empty, Empty))));;

depth_inner tree2;;

let tree3 = Empty;;

depth_inner tree3;;