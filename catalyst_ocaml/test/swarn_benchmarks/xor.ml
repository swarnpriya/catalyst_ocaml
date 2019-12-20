(* Constraint: The integers in the list must be 0s or 1s *)
type pair = Pair of int list * int list 

let rec xor (Pair(l1,l2)) = match l1 with 
 | [] -> []
 | x :: xs -> match l2 with 
              | [] -> []
              | y :: ys -> if x = y then 0 :: xor (Pair(xs,ys)) else 1 :: xor (Pair(xs,ys))


let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list xl