let rec add_zeros n acc = 
	  if n = 0 then acc else let res = add_zeros (n-1) (0 :: acc) in res


let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list xl