type 'a lbt = LEmpty | LNode of 'a * 'a lbt Lazy.t * 'a lbt Lazy.t;;

let rec ltree n = LNode(n, lazy (ltree (2 * n)), lazy (ltree (2 * n + 1)));;

let rec map f = function
  | LEmpty -> LEmpty
  | LNode(x, lazy left_tree, lazy right_tree) -> 
      LNode(f x, lazy (map f left_tree), lazy (map f right_tree));; 

(*preorder traversal*)
let rec ltake (n, tree) = 
  match (n, tree) with 
  | (_, LEmpty) -> []
  | (0, _) -> []
  | (k, LNode(x, lazy left, lazy right)) -> x::(ltake(k - 1, left) @ ltake(k - 1, right));;

ltake (3, (ltree (1)));; 

map (fun x -> x * 2) (ltree (1));;

ltake (3, map ( ( *) 2) (ltree (1)));; 

let finite_tree = LNode("a", lazy (LNode("b", lazy LEmpty, lazy LEmpty)), 
                        lazy (LNode("c", lazy LEmpty, lazy LEmpty)));;

map (fun x -> x ^ "1") finite_tree;;

ltake (10, map (fun x -> x ^ "1") finite_tree);;

map (fun x -> x * 2) (LEmpty);;

ltake (10, map (fun x -> x * 2) (LEmpty));;

