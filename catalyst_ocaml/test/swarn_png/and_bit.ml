type pair_int = Pair_int of int * int 

(* same idea as and_bool *)
(* and_bit : (Pair_int(x2, y2)) : {n | n = 1 <=> ((x2) = 1 /\ (y2) = 1)} *)
(* I need to write the relations over the Pair_int *)
let and_bit l = match l with 
    | Pair_int (x2, y2) -> match (x2, y2) with 
     | (1,1) -> 1
     | (1, 0) -> 0
     | (0,1) -> 0
     | (0, 0) -> 0