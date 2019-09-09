 let eq x1 y1 = true
   exception TestExp
   let raise ex = 
   		[]


 let and_fun x1 y1 = match x1 with 
     | true -> match y1 with 
               | true -> true 
               | false -> false
     | false -> false


 let or_fun x1 y1 = match x1 with 
     | true -> match y1 with 
               | true -> true 
               | false -> true
     | false -> match y1 with 
                | true -> true 
                | false -> false


   let check_hd e l = match l with 
    | [] -> false 
    | x :: xs -> let y = (eq x e) in 
                 if y then true else false


 let rec get_next_element e1 e1' l1 = 
  match l1 with 
    | [] -> false
    | x :: xs -> let y = (eq x e1) in 
                 let z = (check_hd e1' xs) in 
                 let r = and_fun y z in 
                 if r
                 then true 
                 else let rs = (get_next_element e1 e1' xs) in rs


(* or returns any integer that has a digit (a) followed by digit (c or d) *)
let rec reg_or e2 e2' e2'' l2 = 
	          let y = (get_next_element e2 e2' l2) in 
	          let z = (get_next_element e2 e2'' l2) in 
	          let r = (or_fun y z) in 
	          if r
              then l2
              else raise TestExp