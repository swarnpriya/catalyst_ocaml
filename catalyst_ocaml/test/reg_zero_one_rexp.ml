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

 let not_fun x1 = match x1 with 
     | true -> false 
     | false -> true 

 let get_tail l = match l with 
     | [] -> []
     | x :: xs -> xs

 let check_hd e l' = match l' with 
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

let rec get_next_next_element e2 e2' l2 = 
  match l2 with 
    | [] -> false
    | x :: xs -> let y = (eq x e2) in 
                 let t = (get_tail xs) in 
                 let z = (check_hd e2' t) in
                 let r = and_fun y z in 
                 if r
                 then true
                 else let rs = get_next_next_element e2 e2' xs in rs

(* zero_one returns the integer that has a digit (a) followed by zero or one digit (b) *)
let rec reg_zero_one e3 e3' l3 = 
               let x = (get_next_element e3 e3' l3) in 
               let y = (get_next_next_element e3 e3' l3) in 
               let z = not_fun y in 
               let ra = and_fun x z in 
               let ro = not_fun x in 
               let r = or_fun ra ro in 
               if r 
               then l3
               else raise TestExp