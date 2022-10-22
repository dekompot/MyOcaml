let root3 a = 
  let precision = Float.pow 10. (-15.) in 
    let rec root3Inner (a, x) = 
      if Float.abs(Float.pow x 3. -. a) < precision *. Float.abs(a)
        then x 
    else root3Inner(a, x +. (a /. (x *. x) -. x) /. 3.)
  in let initX = if a > 1. then a /. 3. else a
  in root3Inner(a, initX);;

root3 8.;;
root3 27.;;
root3 100.;;