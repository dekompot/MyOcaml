let rec ones = 1::ones;;

ones == List.tl ones;;

let rec clist = 1::2::3::4::5::clist;;

0::clist;;

[6; 7]@clist;;

