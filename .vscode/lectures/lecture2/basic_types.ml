let print_hello s : unit = print_string ("Hello " ^ s);;

print_hello "Julia";;

let de_morgan_law = 
  let a = false in 
  let b = a and a = true 
in a || b;;

let is_true_bigger = true > false;;

let x_mod_2 x = x mod 2;;

x_mod_2 24;;

2. -. 4.;;
2. +. 4.;;
3. *. 4.;;
10. /. 4.;;
2. ** 3.;;

ceil 3.44;;
floor 3.99;;

float_of_int 5;;
int_of_float 3.5;;

print_string "hello world";;

