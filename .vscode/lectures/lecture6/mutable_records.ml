type point = {wx: float; mutable wy: float};;

let p = {wx = 0.0; wy = 1.0};;

p.wy <- 3.0;;

p;;


let increase q = q.wy <- q.wy +. 1.;;

let p1 = {wx = 0.0; wy = 1.0};;

increase p1;;

p1;;