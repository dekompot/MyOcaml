(*serio o to chodzilo???*)
let rec palindrome l = 
  l = List.rev l;;

let xs = [1; 2; 3];;

xs = List.rev (List.rev xs);;

palindrome ['a'; 'b'; 'b'; 'a'];;
palindrome ['a'; 'b'; 'c'; 'b'; 'a'];;
palindrome ['a'];;
palindrome [];;
palindrome ['x'; 'y'; 'z'; 'x'];;
palindrome ['x'; 'y'; 'z'; 'z'; 'x'];;