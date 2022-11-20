(*use ~label:expression*)
let minusTimes ~m_n:(m, n) ~times:times = times * (m - n);;

minusTimes ~times:4 ~m_n:(5, 2);;

minusTimes ~times:3;;

let minusTimes' ~m_n:(m, n) ~times = times * (m - n);;

let times = 2;;

minusTimes' ~times ~m_n:(5, 4);; 

minusTimes' (5, 4) 3;;

