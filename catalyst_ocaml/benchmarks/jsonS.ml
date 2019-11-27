(* JaveScript Object Notation *)
(* JSON is a collection of values where values can be integer, list of integers, string or boolean *)
type v =  | Intv of int 
          | Listv of int list  
          | Bool of bool 

type pair = Pair of int * v

type pairList = 
           | E 
           | LCons of pair * pairList
           | LJCons of int * pairList
           | LJJCons of pairList * pairList

type json = pairList

type list_json = | EL
                 | List_v of v list 
                 | List_paitList of pairList list
                  

let rec concat l1 l2 = 
match l1 with
    [] -> l2
  | x1::xs1 ->  let temp1 = concat xs1 l2 in
                let temp2 = x1::temp1 in 
                temp2

let rec get_first_element_pairlist l3 = match l3 with 
   | E -> []
   | LCons (Pair(x, y), ps) -> let r = get_first_element_pairlist ps in 
                               x :: r
   | LJCons (x, ps) -> [x]
   | LJJCons (x, ps) -> concat (get_first_element_pairlist x) (get_first_element_pairlist ps) 

let rec get_second_element_pairlist l4  = match l4 with 
   | E -> EL
   | LCons (Pair(x, y), ps) -> let r = get_second_element_pairlist ps in 
                               List_v (concat [y] r)
   | LJCons (x, ps) -> List_pairList [ps]
   | LJJCons (x, ps) -> List_pairList [ps]

let rec length_list l = match l with 
  | [] -> 0
  | x :: ls -> let r = length_list ls in 
               let result = r + 1 in 
               result


let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list l

let print_value v = match v with 
  | Intv n -> print_int n 
  | Listv l -> print_list l 
  | Bool b -> Printf.printf "%B" b 

let print_int_value_pair (Pair (p, q)) = print_string "(" ; print_int p; print_string ","; print_value q; print_string ")"


let rec print_pair_list l = match l with 
   | E -> ()
   | LCons (e,l) -> print_int_value_pair e ; print_string " " ; print_pair_list l 
   | LJCons (e, l) -> print_string "("; print_int e; print_string ","; print_pair_list l; print_string ")"
   | LJJCons (e, l) -> print_string "("; print_pair_list e; print_string ","; print_pair_list l; print_string ")"
(* Examples *)

(* (1) Empty : E
(2) [(1, 10); (2, 20)] :
    (LCons (Pair (1, Intv 10), (LCons (Pair (2, Intv 20), E))))
(3) [(1, [(2, 20); (3, 30)])] :
    (LJCons (1, (LCons (Pair (1, Intv 10), (LCons (Pair (2, Intv 20), E))))))
(4) [(4, 40), (1, [(2, 20); (3, 30)])] :
    (LCons (Pair (4, Intv 40), LJCons (1, (LCons (Pair (1, Intv 10), (LCons (Pair (2, Intv 20), E)))))))
(5) [(1, [(2,20), (3,30)]) , (4, 40)]
    (LJJCons  (LJCons (1, (LCons (Pair (1, Intv 10), (LCons (Pair (2, Intv 20), E))))), LCons (Pair (4, Intv 40), E)))*)



(* let rec get_second_element_pairlist l4 = match l4 with 
   | E -> []
   | LCons (Pair(x, y), ps) -> let r = get_second_element_pairlist ps in 
                               y :: r
   | LJCons ((x, y), ps) -> let r = get_first_element_pairlist ps in 
                            y :: r *)


(* let rec unzip l5 = let r1 = get_first_element_pairlist l5 in 
                   let r2 = get_second_element_pairlist l5 in 
                   Pair (r1, r2) *)

(* type pairList = 
           | E 
           | LCons of pair * pairList
           | LConsJ of 


type j = pairList

type value = | Val of v 
             | Val' of j

type json' = 

[(s1, 1); (s2, 2)]

(* and json = pairList*)

(*
(* pair represents the pair of attributes and its correspinding values *)
(* string here represents the name and value represents the value for that attribute *)
type pair = Pair of string * value 

(* json is a list of all information *)
type pairList = 
             E 
           | LCons of pair * pairList

type json = pairList *)

(* Zip *)
 let rec zip l1 l2 = match l1 with 
   | [] -> E
   | y :: ys -> match l2 with 
           | [] -> E
           | x :: xs -> let result = zip ys xs in 
                         LCons ((Pair (y,x)), result)

(* unzip *)
let fst (Pair (x, y)) = x 

let snd (Pair (x, y)) = y 

let rec get_first_element_pairlist l3 = match l3 with 
   | E -> []
   | LCons (Pair(x, y), ps) -> let r = get_first_element_pairlist ps in 
                               x :: r

let rec get_second_element_pairlist l4 = match l4 with 
   | E -> []
   | LCons (Pair(x, y), ps) -> let r = get_second_element_pairlist ps in 
                               y :: r

let rec unzip l5 = let r1 = get_first_element_pairlist l5 in 
                   let r2 = get_second_element_pairlist l5 in 
                   Pair (r1, r2)

(***** Operations *******)
(* Json parse operation takes a json type and returns list of only values corresponding to each field *)
let rec json_parse j = match j with 
   | E -> E
   | LCons (Pair(x,y), xl) -> let uz = unzip j in
                              let z = zip (fst j) (snd j) in 
                              z


(*let print_int_pair (Pair (p, q)) = print_string "(" ; print_int p; print_string ","; print_int q; print_string ")"*)

let rec print_list l = match l with 
   | [] -> ()
   | x :: xl -> print_int x ; print_string ";" ; print_list l

let print_value v = match v with 
  | Intv n -> print_int n 
  | Listv l -> print_list l 
  | Stringv s -> print_string s
  | Bool b -> Printf.printf "%B" b 
  | Null -> ()

let print_string_int_pair (Pair (p, q)) = print_string "("; print_string p; print_string ","; print_value q; print_string ")"

let rec print_pair_list l = match l with 
   | E -> ()
   | LCons (e,l) -> print_string_int_pair e ; print_string " " ; print_pair_list l *) 

