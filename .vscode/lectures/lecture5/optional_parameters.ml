(*syntax ?(label = expression), it cannot be the last argument*)

let minus ?(m = 5) n = m - n;;

minus 3;;

minus ~m:6 4;;