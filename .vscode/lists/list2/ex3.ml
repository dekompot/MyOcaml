let root3 a =  
  let rec root3Inner a x = 
    if Float.abs(Float.pow x 3. -. a) < Float.pow 10. (-15.) *. 
      Float.abs(a) then x 
  else root3Inner a (x +. (a /. (x *. x) -. x) /. 3.)
  in root3Inner a (if a > 1. then a /. 3. else a);;

root3 8.;;
root3 (-27.);;
root3 100.;;