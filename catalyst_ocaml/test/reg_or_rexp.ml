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


 let rec get_next_element e e' l = 
  match l with 
    | [] -> false
    | x :: xs -> let y = (eq x e) in 
                 let z = (check_hd e' xs) in 
                 let r = and_fun y z in 
                 if r
                 then true 
                 else let rs = (get_next_element e e' xs) in rs


(* or returns any integer that has a digit (a) followed by digit (c or d) *)
let rec reg_or e e' e'' l = 
	          let y = (get_next_element e e' l) in 
	          let z = (get_next_element e e'' l) in 
	          let r = (or_fun y z) in 
	          if r
              then l
              else raise TestExp