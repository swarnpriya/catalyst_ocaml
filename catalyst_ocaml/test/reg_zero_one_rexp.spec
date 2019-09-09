relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Robs = Rob*;

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)} };

relation Rnext_hd nil = {()}
                  | (cons (x, xs)) = (Rhd(xs));

relation Rnext_next_hd (nil) = {()}
                  | (cons (x, xs)) = (Rnext_hd(xs));

and_fun : x1 -> y1 -> {veq | [veq=true] <=> (({(x1)}={(true)}) /\ ({(y1)}={(true)}))};

or_fun : x1 -> y1 -> {veq | [veq=false] <=> (({(x1)}={(false)}) /\ ({(y1)}={(false)}))};

not_fun : x -> {veq | [veq=true] <=> ({(x1)}={(false)})};

get_tail : l -> {l' | Rmem(l') C= Rmem(l) /\ Robs(l') C= Robs(l)};

check_hd : e -> l' -> {b | [b=true] <=> Rhd(l) = {(e)}};

get_next_element : e1 -> e1' -> l1 -> {b | [b=true] <=> ((Rhd(l1) = {(e1)}) /\ (Rnext_hd(l1) = {(e1')}))};

get_next_next_element : e2 -> e2' -> l2 -> {b | [b=true] <=> ((Rhd(l2) = {(e2)}) /\ (Rnext_next_hd(l2) = {(e2')}))};


reg_zero_one : e3 -> e3' -> l3 -> {l | Rmem(l) = Rmem(l3) /\
                                     Rhd(l) = {(e3)} /\
                                     ((Rnext_hd(l) = {(e3')} /\ not (Rnext_next_hd(l) = {(e3')})) \/
                                      not (Rnext_hd(l) = {(e3')}))};