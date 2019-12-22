
let rec last_element l = match l with 
 | nil -> nil
 | x :: xl -> let next = last_element xl in 
              if xl = nil
              then (x :: nil)
              else next


