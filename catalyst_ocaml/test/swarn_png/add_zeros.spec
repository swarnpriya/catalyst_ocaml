relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

relation Rlen (nil) = (0) | (cons(x,xs)) =  ((1) + Rlen(xs)); 

add_zeros : n -> acc -> {v | [v = true] <=> {(0)} C Rmem(l)};






