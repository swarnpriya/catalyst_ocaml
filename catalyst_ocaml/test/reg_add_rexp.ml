 let rec get_next_element e e' l = 
  match l with 
    | [] -> false
    | [x] -> false
    | x :: xs -> if x = e && check_hd e' xs 
                 then true 
                 else (get_next_element e e' xs)

 (* add returns the integer that has a digit (a) followed by one or more digit (b) *)
  let rec reg_add e e' l = if (get_next_element e e' l)
               then l
               else failwith "parse error"