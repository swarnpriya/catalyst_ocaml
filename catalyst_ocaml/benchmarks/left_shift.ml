let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                temp2


let shift_left_one_bit l2 = match l2 with 
| [] -> []
| x :: xs -> concat xs [0]


let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list xl