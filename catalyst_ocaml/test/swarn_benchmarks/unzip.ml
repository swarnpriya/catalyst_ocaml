type pair = Pair of int * int

type pairOfList = Pair of int list * int list  

type pairList = 
             E 
         | LCons of pair * pairList

let fst (Pair (x, y)) = x 

let snd (Pair (x, y)) = y 

let rec get_first_element_pairlist l3 = match l3 with 
   | E -> []
   | LCons (Pair(x, y), ps) -> let r = get_first_element_pairlist ps in 
                               x :: r

let rec get_second_element_pairlist l4 = match l4 with 
   | E -> []
   | LCons (Pair(x, y), ps) -> let r = get_second_element_pairlist ps in 
                               y :: r

let rec unzip l5 = let r1 = get_first_element_pairlist l5 in 
                   let r2 = get_second_element_pairlist l5 in 
                   Pair (r1, r2)

let rec print_list l = match l with 
   | [] -> ()
   | x :: xs -> print_int x ; print_string " " ; print_list xs

let print_pair_of_list (Pair (l1, l2)) = print_string "(" ; print_list l1; print_string ","; print_list l2 ; print_string ")"

(* Test case 
print_list (get_first_element_pairlist (LCons ((Pair(2,3)), (LCons ((Pair(2,4)), E)))));;
print_list (get_second_element_pairlist (LCons ((Pair(2,3)), (LCons ((Pair(2,4)), E)))))
print_pair_of_list (unzip (LCons ((Pair(2,3)), (LCons ((Pair(2,4)), E))))) ;; *)
