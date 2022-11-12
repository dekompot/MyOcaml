type 'param param_i_x = int * 'param;;

type param_i_f = float param_i_x;;

let x = (3, 3.14);;

let (x: param_i_f) = (3, 3.14);;

let (x: 'a param_i_x) = (3, 3.14);;