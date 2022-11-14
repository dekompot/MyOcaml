type color = ..;;

type color += Black;;
type color += White | Red;;

let print_color col = 
  match col with 
  | Black -> "black"
  | White -> "white"
  | Red -> "red"
  | _ -> "unknown color";;

type color += Green;;

print_color Green;;