(*Product types - the name comes from association with cartesian product of two sets A and B. We called them tuples*)
(* '*' is a type constructor*)
type point = float * float;;

(*Sum types - they generalize what is known as enum, mathematically - a disjoint set*)
(*kolor and karta are parameterless types constructors*)
type kolor = Trefl | Karo | Kier | Pik;;

(*Sum types can attach values to sum type members*)
(*types are parameterless, Blotka, Walet, Dama, Krol, As are 1-argument values constructors*)
type karta = Blotka of kolor * int | Walet of kolor  
  | Dama of kolor | Krol of kolor | As of kolor;;

(*types are recursive by default*)
type colour = 
| Red | Green | Blue | RGB of float*float*float | Mix of float*colour*colour;;

let mix1 = Mix (0.5, Red, Mix(0.35, Blue, Green));;

let rec rgb_of_colour = function 
| Red -> (1.0, 0.0, 0.0)
| Green -> (0.0, 1.0, 0.0)
| Blue -> (0.0, 0.0, 1.0)
| RGB (r, g, b) -> (r, g, b)
| Mix (p, a, b) ->
    let (r1, g1, b1) = rgb_of_colour a 
    in let (r2, g2, b2) = rgb_of_colour b 
    in let mix x y = p *. x +. (1. -. p) *. y 
    in (mix r1 r2, mix g1 g2, mix b1 b2);;

rgb_of_colour (RGB(0.25, 0.5, 0.25));;

rgb_of_colour (Mix(0.5, Green, Blue));;

let rec range a b = if a > b then [] else 
  b::range a (b - 1);;
  
let allCards kol = 
  let figures = [Walet kol; Dama kol; Krol kol; As kol] 
  and blotkas = List.map (fun n -> Blotka(kol, n)) (range 2 10)
  in figures @ blotkas;; 

allCards Karo;;

(*List type must be homogenous - to make it heterogenous, we need to 
   define our own type*)
(*this is an example of polymorphic sum type*)
(*User-defined constructors must always start with a capital letter*)
(*this is referred to as type constructor, because it can be used to create new monomorphic types, like int * string*)
type ('a, 'b) ab = A of 'a | B of 'b;;

type 'a option = Some of 'a | None;;

let opstr: string option = Some "aa";;
let opint: int option = None;;

let lint = [1; 2; 3];;
let lstr = ["Alice"; "has"; "cat"];;

let heterolist = List.map (fun i -> A i) lstr @ List.map(fun s -> B s) lint;;

heterolist;;

let rec concat_and_add = function 
| [] -> ("", 0)
| h::t -> match (h, concat_and_add t) with 
          | (A str, (s, n)) -> (s ^ str, n)
          | (B num, (s, n)) -> (s, num + n);;

concat_and_add heterolist;;