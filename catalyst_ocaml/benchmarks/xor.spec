relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;
relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 
relation Rlentail (nil) = (0) | (cons(x, xs)) = Rlen(xs);
relation Rhdn (nil) = (0) | (cons(x,xs)) = (x);


xor : l1 -> l2 -> {v | Rlen(v) = Rlen(l1) /\
                       Rlen(v) = Rlen(l2) /\
                       ((Rhdn(v) = Rhdn(l1) /\ not (Rhdn(v) = Rhdn(l2))) \/
                        (Rhdn(v) = Rhdn(l2) /\ not (Rhdn(v) = Rhdn(l1))))}