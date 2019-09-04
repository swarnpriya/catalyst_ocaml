let rec get_next_element e e' l = 
  match l with 
    | [] -> false
    | [x] -> false
    | x :: xs -> if x = e && check_hd e' xs 
                 then true 
                 else (get_next_element e e' xs)

(* or returns any integer that has a digit (a) followed by digit (c or d) *)
let rec reg_or e e' e'' l = if (get_next_element e e' l || get_next_element e e'' l) 
              then l
              else failwith "parse error"