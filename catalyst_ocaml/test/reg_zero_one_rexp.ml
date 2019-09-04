let rec get_next_element e e' l = 
  match l with 
    | [] -> false
    | [x] -> false
    | x :: xs -> if x = e && check_hd e' xs 
                 then true 
                 else (get_next_element e e' xs)

let rec get_next_next_element e e' l = 
  match l with 
    | [] -> false
    | x :: xs -> if x = e && (check_hd e' (List.tl xs))
                 then true
                 else get_next_next_element e e' xs 



(* zero_one returns the integer that has a digit (a) followed by zero or one digit (b) *)
  let rec reg_zero_one e e' l = if ((get_next_element e e' l) && (not (get_next_next_element e e' l))) ||
               not (get_next_element e e' l)
               then l
               else failwith "parse error"