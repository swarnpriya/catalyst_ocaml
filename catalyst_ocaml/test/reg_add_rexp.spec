relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Robs = Rob*;

relation Rnext_hd nil = {()}
                  | (cons (x, xs)) = (Rhd(xs));

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)} };

and_fun : x1 -> y1 -> {veq | [veq=true] <=> (({(x1)}={(true)}) /\ ({(y1)}={(true)}))};

check_hd : e -> l -> {b | [b=true] <=> Rhd(l) = {(e)}};

get_next_element : e1 -> e1' -> l1 -> {b1 | [b1=true] <=> ((Rhd(l1) = {(e1)}) /\ (Rnext_hd(l1) = {(e1')}))};

reg_add : e2 -> e2' -> l2 -> {l | (Rmem(l) = Rmem(l2)) /\
                                  (Rhd(l) = {(e2)}) /\
                                  (Rnext_hd(l) = {(e2')})};