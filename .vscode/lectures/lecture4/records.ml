(*Records are named tuples*)
type complex = {re: float; im: float};;

let c = {re=2.0; im=3.0};;

c = {im=3.0; re=2.0};;

(*use .dot notation to access fields*)
let add_complex c1 c2 = 
  {re=c1.re +. c2.re; im=c1.im +. c2.im};;

(*or use pattern matching*)
let mult_complex {re=x1; im=y1} {re=x2; im=y2} = 
  {re=x1 *. x2 -. y1 *. y2; im=x1 *. y2 +. x2 *. y1};;

mult_complex c c;;

type aRec = {p1: bool; re: float};;

let rec1 = {re=5.6; p1=false};;

(*pattern does not need to have all
   fields written out*)
let h {re=x} = x;;


(* We can create records differing from 
   other records just by given fields values
   {ident with field1=val1; ...; fieldk=valk}*)

let rec2 = {rec1 with re=0.};;

