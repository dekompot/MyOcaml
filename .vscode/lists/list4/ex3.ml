type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let breadthBT bt = 
  let rec bfs queue = 
    match queue with 
    | [] -> []
    | Empty::t -> bfs t
    | Node(x, lt, rt)::t -> x::bfs(t@[lt; rt])
  in bfs [bt];;


let tree = Node(1, 
              Node(2, Node(4, Empty, Empty), Empty), 
              Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty));;

breadthBT tree;;