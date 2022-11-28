for i = 1 to 10 do print_int i; print_string " " done;

print_newline();;

let r = ref 1 
in begin 
  while !r < 11 do 
    print_int !r;
    print_string " ";
    r := !r + 1;
  done;
  print_newline();
end;;


let x = ref (-5);;

if !x > 0 then x:= -(!x);;

if !x < 0 then x:= -(!x);;

(*the expected type is a type unit because the else branch is omitted*)
if !x > 0 then 10;;