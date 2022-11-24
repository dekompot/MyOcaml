let v = [|2.56; 3.45; 8.74|];;

let v = Array.make 3 3.44;;

v.(1);;

v.(0) <- 2.34;;

v;;

v.(-3) +. 5.0;;

(*vector.(position)
  vector.(postion) <- new_value*)

let str_vector = [|"a"; "b"; "c"|];;

Array.fold_left (fun acc x -> (acc +. x)) 0.0 v;;

Array.fold_right (fun s acc -> (acc ^ s)) str_vector "";;

Array.to_list str_vector;;
Array.of_list [1; 2; 3];;

let vv = Array.make 3 0;;

let nested_v = Array.make 3 vv;;

(*memory is shared when it comes to references*)
nested_v.(0) == vv;;
nested_v.(1) == vv;;
nested_v.(2) == vv;;

vv.(0) <- 1;;

nested_v;;

let m' = Array.init 3 (function _ -> Array.make 3 0);;

m'.(0).(0) <- 1;;

m';;

let matrix = [|[|1|]; 
              [|2; 2|]; 
              [|3; 3; 3|]|];;

matrix.(1);;          