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

get_next_element : e -> e' -> l -> {b | [b=true] <=> ((Rhd(l) = {(e)}) /\ (Rnext_hd(l) = {(e')}))};

reg_add : e -> e' -> l1 -> {l | (Rmem(l) = Rmem(l1)) /\
                                (Rhd(l) = {(e)}) /\
                                (Rnext_hd(l) = {(e')})};