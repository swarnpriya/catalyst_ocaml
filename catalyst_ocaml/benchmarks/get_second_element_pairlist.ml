
type pair = Pair of int * int

type pairOfList = Pair of int list * int list 

type pairList = 
             E 
         | LCons of pair * pairList

let fst (Pair (x1, y1)) = x1 

let snd (Pair (x2, y2)) = y2 



let rec get_second_element_pairlist l3 = match l3 with 
   | E -> []
   | LCons (Pair(x3, y3), ps) -> let r = get_first_element_pairlist ps in 
                               y3 :: r