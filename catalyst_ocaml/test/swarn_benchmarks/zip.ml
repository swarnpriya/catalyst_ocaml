type pair = Pair of int * int 

type pairList = 
             E 
         | LCons of pair * pairList

exception PairTestExp
   (*let raise ex = 
   		E*)

 let rec zip l1 l2 = match l1 with 
   | [] -> E
   | y :: ys -> match l2 with 
           | [] -> E
           | x :: xs -> let result = zip ys xs in 
                         LCons ((Pair (y,x)), result)
   

(*let print_int_pair (Pair (p, q)) = print_string "(" ; print_int p; print_string ","; print_int q; print_string ")"

let rec print_pair_list l = match l with 
   | E -> ()
   | LCons (e,l) -> print_int_pair e ; print_string " " ; print_pair_list l*)