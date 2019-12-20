(* Specification:
   (1) Last bit should be zero 
   (2) Rest of the bits should be same as input *)
let shift_left_one_bit l2 = match l2 with 
| [] -> []
| x :: xs -> concat xs [0]