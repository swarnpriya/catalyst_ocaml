(* JaveScript Object Notation *)
(* JSON is a collection of values where values can be integer, list of integers, string or boolean *)
type value = 
            | Intv of int 
            | Listv of int list  
            | Stringv of string 
            | Bool of bool 
            | Null

(* pair represents the pair of attributes and its correspinding values *)
(* string here represents the name and value represents the value for that attribute *)
type pair = Pair of string * value 

(* json is a list of all information *)
type pairList = 
             E 
           | LCons of pair * pairList

type json = pairList 

(***** Operations *******)
(* Json parse operation takes a json type and returns list of only values corresponding to each field *)
let rec json_parse j = match j with 
   | E -> E
   | LCons (Pair(x,y), xl) -> LCons (Pair(x,y), xl)


(* Access fields in jason *)
let rec access_by_field j f = match j with 
   | E -> Null
   | LCons (Pair(x,y), xl) -> (* need to use something else for =*) if x = f then y else access_by_field xl f


(* Modify the value associcated with the field *)
let rec modify_by_field j f v = match j with 
   | E -> E
   | LCons (Pair(x,y), xl) -> if x = f then LCons (Pair(x,v), xl) else modify_by_field xl f v


(* Delete any field *)
let rec delete_by_field j f = match j with
   | E -> E
   | LCons (Pair(x,y), xl) -> if x = f then xl else LCons (Pair(x,y), delete_by_field xl f)

(* Add any field *)
let rec delete_by_field j f = match j with
   | E -> E
   | LCons (Pair(x,y), xl) -> if x = f then xl else LCons (Pair(x,y), delete_by_field xl f)


(*let print_int_pair (Pair (p, q)) = print_string "(" ; print_int p; print_string ","; print_int q; print_string ")"*)

(* let rec print_list l = match l with 
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

