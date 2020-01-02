(* Specification:
   (1) Last bit should be zero 
   (2) Rest of the bits should be same as input *)

(* We have the specification written by Ashish *)
let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                temp2

let shift_left_one_bit l3 = match l3 with 
| [] -> []
| x :: xs -> let res = concat xs [0] in res 

(*let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list xl*)