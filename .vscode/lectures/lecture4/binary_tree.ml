type 'a bt = 
| Empty
| Node of 'a * 'a bt * 'a bt;;

let tree = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Node(4, Empty, Empty));;

let rec count tree = 
  match tree with 
  | Empty -> 0
  | Node (_, tL, tR) -> 1 + count tL + count tR;;

count tree;;

let rec preorder tree = 
  match tree with
  | Node (v, tL, tR) -> v :: (preorder tL @ preorder tR) 
  | Empty -> [];;

preorder tree;;

let preorder' tree = 
  let rec preord = function 
  | (Empty, labels) -> labels
  | (Node(v, tL, tR), labels) -> v::preord(tL, preord(tR, labels))
in preord(tree, []);;

preorder' tree;;

(*Functions that use @ have quadratic complexity*)
let rec inorder tree = 
  match tree with 
  | Node(v, tL, tR) -> inorder tL @ v::(inorder tR)
  | Empty -> [];;

inorder tree;;

let inorder' tree = 
  let rec inord = function 
  | (Empty, labels) -> labels
  | (Node(v, tL, tR), labels) -> inord(tL, v::inord(tR, labels))
in inord(tree, []);;

inorder' tree;;

let rec postorder tree = 
  match tree with 
  | Empty -> []
  | Node(v, tL, tR) -> postorder tL @ postorder tR @ [v];;

postorder tree;;
let postorder' tree = 
  let rec postord = function 
  | (Empty, labels) -> labels 
  | (Node(v, tL, tR), labels) -> postord(tL, postord(tR, v::labels))
in postord(tree, []);;

postorder' tree;;

let rec list2bst xs = 
  let rec insert2bst = function
  | (k, Node(r, lt, rt)) -> 
    if r < k then Node(r, insert2bst(k, lt), rt)
    else if r > k then Node(r, lt,insert2bst(k, rt))
    else failwith "Duplicated key"
  | (k, Empty) -> Node(k, Empty, Empty)
  in match xs with 
  | h::t -> insert2bst(h, list2bst t)
  | [] -> Empty;;

list2bst [6; 4; 9; 2; 5];;