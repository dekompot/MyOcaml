let f1 ((x, y), z) = (x, y, (x, y), z);;

let f2 ((x, y) as p, z) = (x, y, p, z);;

let k4_3 (_, _, v, _) = v;;

let xy = (1, 2);;

let k = (xy, 3);;

let w1 = f1 k;;

xy == k4_3 w1;;

let w2 = f2 k;;

xy == k4_3 w2;;

let rec tails xs = 
  match xs with 
  | _::t -> xs :: tails t
  | [] -> [[]];;

let xs = [1; 2; 3];;

let ys = tails xs;;

List.hd ys == xs;;

List.hd (List.tl ys) == List.tl xs;;

List.hd (List.tl (List.tl ys)) == List.tl (List.tl xs);;

