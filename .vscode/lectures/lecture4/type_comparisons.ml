(*The order is defined by the 
   order of definition*)
type kolor = Trefl | Karo | Kier | Pik;;

type 'a tree = Leaf of 'a | Branch of 'a tree * 'a tree;;

Trefl < Karo;;

min Pik Karo;;

Leaf 4 < Branch (Leaf 0, Leaf 4);;

Branch (Leaf 0, Leaf 1) < Branch (Leaf 0, Leaf 2);;