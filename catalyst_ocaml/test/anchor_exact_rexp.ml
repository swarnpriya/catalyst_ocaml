  let rec compare_each_elm_equality l l' = match (l, l') with 
     | [], [] -> true 
     | [], _ -> false
     | _, [] -> false
     | x :: xs, y :: ys -> if x = y 
                           then compare_each_elm_equality xs ys 
                           else false 

(* anchor_exact returns any integer that has digit provides as argument in it *)
let rec anchor_exact e l = if compare_each_elm_equality e l
                            then l 
                            else failwith "parse error" 