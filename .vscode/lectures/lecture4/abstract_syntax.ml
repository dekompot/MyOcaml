type expression = 
| Const of float
| Var of string 
| Sum of expression * expression
| Diff of expression * expression
| Prod of expression * expression
| Quot of expression * expression

exception Unbound_variable of string;;

let rec eval env exp = 
  match exp with 
  | Const c -> c 
  | Var v -> (try List.assoc v env 
            with Not_found -> raise(Unbound_variable v))
  | Sum(a, b) -> eval env a +. eval env b
  | Diff(a, b) -> eval env a -. eval env b
  | Prod(a, b) -> eval env a *. eval env b
  | Quot(a, b) -> eval env a /. eval env b;;
  
eval [("x", 1.0); ("y", 3.14)] (Prod(Sum(Var "x", Const 2.0), Var "y"));;

eval [("x", 1.0); ("z", 3.14)] (Prod(Sum(Var "x", Const 2.0), Var "y"));;