relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};

relation Robs = Rob*;

relation Roas = Roa*;

relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 

set_msb : l -> {v | Rlen(v) = Rlen(l) /\
                    Rhd(v) = {(1)}};