let set_msb l = match l with 
| [] -> []
| x :: xs -> if x = 1 then l else 1 :: xs 


(*let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list xl*)