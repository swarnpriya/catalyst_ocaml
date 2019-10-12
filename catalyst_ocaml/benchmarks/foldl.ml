type pair = Pair of int * int 

let eq x1 y1 = true
   
exception TestExp
   let raise ex = 
   		[]

let rec fold_left f accu l =
  match l with
    [] -> accu
  | a::l -> let result1 = f accu a in 
            let result = fold_left f result1 l 
            fold_left f (f accu a) l


(* Properties of fold left *)
(1) It walks through the list from left to right. 
(2) It takes a function f which walks the list from left to right, updating the accu at each step and returns the final value of accu when the list is complete 
(3) The final value depends on the function, it can be the member of the list or cannot be. Can be something entirely different. 