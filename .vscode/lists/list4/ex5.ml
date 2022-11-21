type 'a graph = Graph of ('a -> 'a list);;

let g = Graph 
(function 
  |0 -> [3]
  |1 -> [0; 2; 4]
  |2 -> [1]
  |3 -> []
  |4 -> [0; 2]
  |n -> failwith ("Graph g: node "^string_of_int n^"does not exist"));;

let depth_search (Graph succ) start_node = 
  let rec search visited neighbours = 
    match neighbours with 
    | [] -> ([], visited)
    | h::t -> match h with 
              | h when List.mem h visited -> search visited t 
              | h -> let (s, v) = search (h::visited) (succ h) in 
              let (s2, v2) = search (v) (t) in 
              (h::s@s2, v2)
  in let (order, _) = search [start_node] (succ start_node) in 
  start_node::order;;
  
depth_search g 4;;
  
let g2 = Graph(
  function 
  | 0 -> [1; 2; 3]
  | 1 -> [4]
  | 2 -> [0]
  | 3 -> [0]
  | 4 -> [2; 3; 5]
  | 5 -> []
  | n -> failwith ("Graph g: node "^string_of_int n^"does not exist")
);;
  
depth_search g2 0;;
  
depth_search g2 4;;  

let depth_search_2 (Graph succ) start_node = 
  let rec search queue visited = 
    match queue with 
    | [] -> List.rev visited
    | h::t -> if List.mem h visited then search t visited 
    else search(succ h @ t) (h::visited)
  in search [start_node] [];;

depth_search_2 g 4;;

depth_search_2 g2 0;;
  
depth_search_2 g2 4;; 