(*"zaklad ubezpieczen Spolecznych" -> "ZUS"*)

List.fold_left (fun acc s -> acc ^ (String.uppercase_ascii (String.sub s 0 1))) ("") (String.split_on_char ' ' "zaklad ubezpieczen Spolecznych");; 

String.get "Hello" 0;;

