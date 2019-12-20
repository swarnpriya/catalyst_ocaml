type pair = Pair of int * int 

type pairList = 
             E 
         | LCons of pair * pairList

 let rec dup l1 = match l1 with 
   | [] -> E
   | x :: xs -> let result = dup xs in 
                LCons (Pair(x, x), result)

let print_int_pair (Pair (p, q)) = print_string "(" ; print_int p; print_string ","; print_int q; print_string ")"

let rec print_pair_list l = match l with 
   | E -> ()
   | LCons (e,l) -> print_int_pair e ; print_string " " ; print_pair_list l