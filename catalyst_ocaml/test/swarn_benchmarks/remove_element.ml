let rec remove_element x l = match l with 
  | [] -> []
  | y :: ys -> let z = (x = y) in
               let result = remove_element x ys in 
               if z then result 
                    else y :: result

(*let rec print_list l = match l with 
   | [] -> ()
   | e::l -> print_int e ; print_string " " ; print_list l*)
