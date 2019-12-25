relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

check_if_zero_or_not : l -> {v | [v = true] <=> {(0)} C Rmem(l)};






