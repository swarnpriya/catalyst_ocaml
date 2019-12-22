(* Constraint: The integers in the list must be 0s or 1s *)
type pair_int = Pair_int of int * int 
type pair = Pair of int list * int list 

let rec xor (Pair(l1,l2)) = match l1 with 
 | [] -> []
 | x :: xs -> match l2 with 
              | [] -> []
              | y :: ys -> if x = y then 0 :: xor (Pair(xs,ys)) else 1 :: xor (Pair(xs,ys))


let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list xl

(* We have the specification written by Ashish *)
let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                temp2

(* Specification:
   (1) Last bit should be zero 
   (2) Rest of the bits should be same as input *)
(* Wrote the final spec *)
let shift_left_one_bit l2 = match l2 with 
| [] -> []
| x :: xs -> concat xs [0]


(* wrote final spec *)
let rec list_length l = match l with 
| [] -> 0
| x :: xs -> list_length xs + 1

let and_bool x2 y2 = match x2 with 
     | true -> match y2 with 
               | true -> true 
               | false -> false
     | false -> false 

(* same idea as and_bool *)
(* and_bit : (Pair_int(x2, y2)) : {n | n = 1 <=> ((x2) = 1 /\ (y2) = 1)} *)
(* I need to write the relations over the Pair_int *)
(* Wrote final spec *)
let and_bit (Pair_int(x2, y2)) = match (x2, y2) with 
     | (1,1) -> 1
     | (1, 0) -> 0
     | (0,1) -> 0
     | (0, 0) -> 0


let rec and_list (Pair(l1, l2)) = match l1 with 
| [] -> []
| x :: xs -> match l2 with 
             | [] -> []
             | y :: ys -> let r = and_bit (Pair_int(x, y)) in 
                          let rs = and_list (Pair(xs, ys)) in 
                          r :: rs 

let left_shift_followed_xor (Pair(l1,l2)) = let r = shift_left_one_bit l1 in 
                                            let result = xor (Pair(r, l2)) in 
                                            result

(* This function checks if all the elements in l are zero or not *)
(* Spec: Need to think about Spec *)
let rec check_if_zero_or_not l = match l with 
  | [] -> true
  | x :: xs -> if x = 0 then check_if_zero_or_not xs else false 

(* This function outputs the list containing only n zeros *)
(* Spec: (1) length of ouput list is n 
         (2) All elements of the list is 0 *)
let rec add_zeros n acc = 
	  if n= 0 then acc else add_zeros (n-1) (0 :: acc) 

(* This function sets the most significant bit *)
(* Spec:  (1) The head of output list should be 1 
          (2) Length of input list and output list should be same. *)
(* wrote the final spec *)
let set_msb l = match l with 
| [] -> []
| x :: xs -> if x = 1 then l else 1 :: xs 


(* Specification will be just concerened with the msb bit *)
(* r : c : {v | v = true <=> Rhd{c} = {(1)}} *)
let check_most_significant_bit_set c = 
	let len = list_length c in 
	let r = add_zeros len [] in 
	let r' = set_msb r in 
	let r'' = (and_list (Pair(c, r'))) in 
	let r''' = check_if_zero_or_not r'' in 
	if r''' then false else true 


(* g is one bit more than c *)
let compute_crc_one_step c g = if check_most_significant_bit_set c then left_shift_followed_xor (Pair(c,g)) else shift_left_one_bit c


(* here g is the generator which is fixed *)
(* I have tested it for 8 bits where g is taken as [0;0;0;1;1;1;0;1] *)
(* here n is 7 as the counting starts from 0 and goes to 7 total 8 bits *)
let rec generate_crc n l g = 
	if n>0 then 
	let res1 = compute_crc_one_step l g in 
	let res = generate_crc (n-1) res1 g in 
	res 
    else compute_crc_one_step l g

let compute_png n 
l g = 
	let r = (generate_crc n l g) in 
	let result = concat l r in 
	result 


let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list xl

 (*crc = [1;1;0;0;0;0;1;0] = 0xC2
 g = [0;0;0;1;1;1;0;1] = 0x1D*)
