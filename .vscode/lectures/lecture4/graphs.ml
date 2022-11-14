type 'a graph = Graph of ('a -> 'a list);;

let breadth_search (Graph succ) start_node = 
  let rec search visited queue = 
    match queue with 
    | [] -> []
    | h::t -> if List.mem h visited then search visited t
              else h::search (h::visited) (t @ succ h)
  in search [] [start_node];;
  
let g = Graph 
(function 
  |0 -> [3]
  |1 -> [0; 2; 4]
  |2 -> [1]
  |3 -> []
  |4 -> [0; 2]
  |n -> failwith ("Graph g: node "^string_of_int n^"does not exist"));;

breadth_search g 4;;