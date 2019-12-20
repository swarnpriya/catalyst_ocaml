(*open Pervasives
let equal_equal x1 y1 = Pervasives.equal x1 y1;; *)
(*let int_eq x y = (x = y)*)

(*let eq x y = false*)

let rec exists e l1 = 
	match l1 with 
	| [] -> false 
	| x2 :: xs2 -> let b = (e = x2) in 
	                if b then true else let r = exists e xs2 in r


(*let rec exists e l1 = 
	match l1 with 
	| [] -> false 
	| x2 :: xs2 -> match e with 
	               | true -> match x2 with 
	                         | true -> true 
	                         | false -> let r1 = exists e xs2 in r1 
	               | false -> match x2 with 
	                         | true -> let r2 = exists e xs2 in r2 
	                         | false -> true *)




(*let b = eq e x2 in 
	               if b then true else 
	               let result = exists e xs2 in result*)
	               
	                  

(*Printf.printf "%B" (exists 1 [1;2;3]);;*)

(*let rec exists e l1 = 
	match l1 with 
	  | [] -> false 
	  | x2 :: xs2 -> let y = eq e x2 in 
	               if y then true 
	                    else let b = exists e xs2 in b *)

(*let l = [1;2;3] in 
let result = (exists 1 l) in 
(print_bool result);
*)

	          
