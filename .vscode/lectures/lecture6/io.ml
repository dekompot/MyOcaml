let file = open_in ".vscode/lectures/lecture6/believe.txt";;

print_string (input_line(file));;

let bb = ref Bytes.of_string;;

let isspace ch = (ch = ' ') || (ch = '\t') || (ch = '\n') || (ch = '\r');;

let input_string channel = 
  let s = ref "" and ch = ref (input_char channel)
  in 
    begin
      while isspace !ch do ch := (input_char channel) done;
      while not (isspace !ch) do 
        s := !s^(String.make 1 !ch);
        ch := (input_char channel)
      done;
    !s
    end
  ;;

let file = open_in "C:/OCaml64/home/julia/listy/.vscode/lectures/lecture6/believe.txt";;

print_string (input_string file);;