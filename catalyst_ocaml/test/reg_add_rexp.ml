 let eq x1 y1 = true

 exception TestExp

 let raise ex = 
   		[]

 let and_fun x1 y1 = match x1 with 
     | true -> match y1 with 
               | true -> true 
               | false -> false
     | false -> false

 let check_hd e l = match l with 
    | [] -> false 
    | x :: xs -> let y = (eq x e) in 
                 if y then true else false


 let rec get_next_element e e' l = 
  match l with 
    | [] -> false
    | x :: xs -> let y = (eq x e) in 
                 let z = (check_hd e' xs) in 
                 let r = (and_fun y z) in 
                 if r
                 then true 
                 else let rs = (get_next_element e e' xs) in rs

 (* add returns the integer that has a digit (a) followed by one or more digit (b) *)
 let rec reg_add e e' l = let r = (get_next_element e e' l) in 
               if r
               then l
               else raise TestExp