let rec map f xs = 
  match xs with 
  | [] -> []
  | h::t -> (f h):: map f t;;

map (fun x -> x * x) [1; 2; 3; 4];;

List.map String.length ["Litwo"; "Ojczyzno"; "Moja"];;

