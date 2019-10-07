/- global declaration where Sigma is a Type. These 
   variable remain in scope until the end of the file. -/
variable (α : Type)

/- regExp is a list of Sigma type which can be any type supported in Lean -/
definition alphabets := list α

/- A regular language is a formal language that can be expressed using regular 
   expression. A language recognized by finite automaton.
   Some properties are:
   (1) Empty language is also a regular language.
   (2) For each a ∈ alphabet, {a} is a regular language.
   (3) If A and B are regular languages then A U B (union), A * B (concatenation) and 
       A* (Kleene star) are regular languages. -/
/- Examples:
   Regular expression :  (ab*) (a followed by 0 or more b)
   Regular language : {a, ab, abb, abbb, abbbb, .....} 
   
   Regular expression : v*c* (any no of vowels followed by any no of consonants 
   Regular language : {∅, a, aou, aiou, b, abcd, .....} -/

/- I denoted it as proposition because it says that l s is true for exactly 
   those strings in the language l -/
definition regLang := alphabets α -> Prop 

/- Regular expressions -/
inductive regExp : Type 
 /- empty contains no alphabet -/
 | empty : regExp 
 /- regConcat concatenates two alphabets -/
 | regConcat : alphabets α → alphabets α → regExp
 /- regUnion takes the union of two alphabets -/
 | regUnion : alphabets α → alphabets α → regExp
 /- reg_start returns the alphabets if it starts with the alphabet provided
    as the first argument -/ 
 | regStart : α → alphabets α → regExp
 /- reg_end returns the alphabets if it ends with the alphabet provided 
    as the first argument -/ 
 | regEnd : α → alphabets α → regExp 
 /- reg_end_start returns the alphabets if it starts and ends with the 
    alphabet provided in the argument -/
 | regStartEnd : α → α → alphabets α → regExp
 /- reg_exact returns the alphabets if it exactly matches the alphabets provided 
    in the argument -/ 
 | regExact : alphabets α → alphabets α → regExp 
 /- reg_or returns the alphabets if a followed by b or c in the alphabets where 
    a is the head of the alphabets. Example: abe, acd -/
 | regOr : α → α → α → alphabets α → regExp
 /- reg_add returns the alphabets if a followed by one or more b
    Example: ab, abb, abbb, .... -/
 | regAdd : α → α → alphabets α → regExp 
 /- reg_zero_one returns the alphabets if a followed by zero or one b 
    Example: a, ab, abc -/
 | regZeroOne : α → α → alphabets α → regExp 

/-----------------------------------------------------------               -/
/- Auxillary fucntions-/
/- check for membership -/
/- here decidable_eq is decidable equality check -/
def memAlphabets {α} [decidable_eq α]: α -> alphabets α → bool
  | e list.nil := false 
  | e (x :: xs) := if e = x then true else memAlphabets e xs

/- Returns last element of the list -/
/- inhabited is a witness to the fact that there is an element of type α -/
/- arbitrary is same as default, it doesnt reduce. -/
def lastElement [inhabited α]: alphabets α -> α 
  | list.nil := arbitrary α 
  | [x] := x
  | [x, y] := y
  | (x :: xs) := lastElement xs

/- Returns true if e is the last element of the list -/
def checkLastElement {α} [decidable_eq α] [inhabited α]: α -> alphabets α -> bool 
  | e list.nil := false 
  | e (x :: xs) := if e = (lastElement _ (x :: xs)) then true else false 

/- Check equality for each element -/
def checkEquality {α} [decidable_eq α]: alphabets α -> alphabets α -> bool 
  | [] [] := true
  | [] (x :: xs) := false 
  | (x :: xs) [] := false 
  | (x :: xs) (y :: ys) := if x = y then checkEquality xs ys else false

/- Checks if next element to the head is e' or not -/
def getNextHead {α} [decidable_eq α] [inhabited α]: α -> alphabets α -> bool
  | e [] := false 
  | e (x :: xs) := if e = (list.head xs) then true else false

/- Checks if next-to-next element to head is e' or not -/
def getNextToNextHead {α} [decidable_eq α] [inhabited α] : α -> alphabets α -> bool 
 | e [] := false 
 | e (x :: xs) := if e = (list.head (list.tail xs)) then true else false

/---------------------------------------------/
 /- Operational Semantics -/
 /- Trying to give denotional semantics which means constructing mathematical objects 
    that describe the meaning of expressions from the language. -/
/- Dependent Pattern matching -/
 def opsemRegExp {α} [decidable_eq α] [inhabited α] [inhabited (alphabets α)]: 
  regExp α -> alphabets α 
  | opsemRegExp (empty) := list.nil
  | opsemRegExp (regConcat l l') := list.append l l'
  | opsemRegExp (regUnion l l') := list.union l l'
  | opsemRegExp (regStart e l) := 
    if e = (list.head l) then l else arbitrary (alphabets α)
  | opsemRegExp (regEnd e l) := 
    if (checkLastElement e l) then l else arbitrary (alphabets α)
  | opsemRegExp (regStartEnd e e' l) := 
    if (e = list.head l ∧ checkLastElement e' l) 
    then l else arbitrary (alphabets α)
  | opsemRegExp (regExact l l') := 
    if (checkEquality l l') then l' else arbitrary (alphabets α) 
  | opsemRegExp (regOr e e' e'' l) := 
    if ((e = list.head l) ∧ ((getNextHead e' l) ∨ (getNextHead e'' l)))
    then l else arbitrary (alphabets α)
  | opsemRegExp (regAdd e e' l) := 
    if ((e = list.head l) ∧ getNextHead e' l) then l else 
    arbitrary (alphabets α)
  | opsemRegExp (regZeroOne e e' l) := 
    if ((e = list.head l) ∧ (((getNextHead e' l) ∧ (¬ getNextToNextHead e' l)) 
       ∨ (¬ getNextHead e' l))) then l else arbitrary (alphabets α)
  /- Small step -/
  | opsemRegExp (regConcat l1 l2) := 
    /- l1 takes step to l1' 
       l2 takes step to l2' then 
       opsemRegExp (regConcat l1 l2) --> 
       opsemRegExp (regConcat l1' l2') -/
 





  




