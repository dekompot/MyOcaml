let x = (false, 10);;

let (y, z) = x;;

let (false, y) = x;;

let (true, y) = x;;

let xs = ['A'; 'B'; 'C'];;

let [xs1; xs2; xs3] = xs;;

xs1;;

let h::t = xs;;

let (z, _) = (false, 10);;

let x = (("Smith", 25), true);;

let ((n, w), b) = x;;

let f1 = fun ((x, y), z) -> (x, y, (x, y), z);;

f1 ((2, 5.5), [1; 2]);;

let imply1 pb =
  match pb with
| (false, false) -> true
| (false, true) -> true
| (true, false) -> false
| (true, true) -> true
;;

imply1(1 > 2, true);;

let imply2 pb =
  match pb with
| (true, false) -> false
| _ -> true
;;

imply2(1 > 2, false);;
imply2(3 > 1, true);;

let rec zip (xs, ys) =
  match (xs, ys) with
| (h1::t1, h2::t2) -> (h1, h2)::zip(t1, t2)
| _ -> [];;

zip ([1; 2; 3], ['a'; 'b'; 'c']);;

let rec unzip xs =
  match xs with
| (h1, h2)::t -> let (xs1, xs2) = unzip t in (h1::xs1, h2::xs2)
| _ -> ([], []);;

unzip([(1, 'a'); (2, 'b'); (3, 'c')]);;

let isLatinVowel v =
  match v with
| 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> true
| _ -> false;;

isLatinVowel 'a';;
isLatinVowel 'b';;

let mean p =
  match p with
| (x, y) when x = y -> x
| (x, y) -> (x +. y) /. 2.;;

mean (4., 4.);;
mean (2., 3.);;

let mean_ (x, y) = if x = y then x else (x +. y) /. 2.;;


mean_(4., 4.);;
mean_(2., 3.);;

let xs = [1; 2; 3];;
let ys = xs;;
ys == xs;;

let ys = let h::t = xs in h::t;;
ys == xs;;
(List.tl xs) == (List.tl ys);;

let rec (@) l1 l2 =
  match l1 with
| [] -> l2
| hd::tl -> hd :: (tl @ l2);;

let xs = [1; 2];;
let ys = [3; 4];;
let zs = xs @ ys;;
(*Not all content is copied*)
List.tl (List.tl zs) == ys;;

let rec badAppend l1 l2 =
  match (l1, l2) with
| ([], []) -> []
| ([], h2::t2) -> h2 :: (badAppend [] t2)
| (h1::t1, []) -> h1 :: (badAppend t1 [])
| (h1::t1, h2::t2) -> h1 :: (badAppend t1 l2);;

let zs = badAppend xs ys;;
(*The 2nd list is also copied*)
List.tl (List.tl zs) == ys;;

f1;;

(*as-patterns, layered patterns*)
let ((n, w) as d, b) = x;;

let f2 ((x, y) as p, z) = (x, y, p, z);;

lsx