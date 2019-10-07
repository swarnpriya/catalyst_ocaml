relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Robs = Rob*;

assume raise : ex -> {vex | true};
assume eq : x1 -> y1 -> {veq | [veq=true] <=> {(x1)} = {(y1)}};

relation 

exists_predicate : f -> l -> {v = true | }