   let eq x1 y1 = true
   exception TestExp
   let raise ex = 
   		[]

  let rec mem_check e l = match l with 
     | [] -> false 
     | x :: xs -> let r = eq e x in 
                  if r 
                  then true 
                  else let rs = mem_check e xs in rs 

(* anchor_exact returns any integer that has digit provides as argument in it *)
let rec anchor_exact e l = let r = mem_check e l in 
                            if r 
                            then l 
                            else raise TestExp