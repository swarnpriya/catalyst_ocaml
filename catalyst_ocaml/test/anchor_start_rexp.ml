   let check_hd e l = match l with 
    | [] -> false 
    | x :: xs -> if x = e then true else false

 (* Anchor ^ which matches any integer starting with the argument provided 
     ^The returns the string that starts with The *)
  let rec anchor_start e l = match l with 
      | [] -> []
      | x :: xs -> if (check_hd e l) 
                   then l
                   else failwith "parse error"