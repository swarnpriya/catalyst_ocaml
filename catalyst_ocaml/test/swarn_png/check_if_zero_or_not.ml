let rec check_if_zero_or_not l = match l with 
  | [] -> true
  | x :: xs -> let res = check_if_zero_or_not xs in 
               if x = 0 then res else false 