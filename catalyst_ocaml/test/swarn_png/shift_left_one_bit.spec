relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};

relation Robs = Rob*;

relation Roas = Roa*;

relation Rlast nil = {()}
               | (cons (x, nil)) = {(x)}
               | (cons (x, xs)) = Rlast (xs);

relation Rsnd nil = {()}
              | (cons(x, xs)) = Rhd (xs);


concat : l1 -> l2 -> { l | Rmem(l) = Rmem(l1) U Rmem(l2) /\ 
										Robs(l) = Robs(l1) U Robs(l2) U (Rmem(l1) X Rmem(l2))};

shift_left_one_bit : l3 -> {v | Rmmem(v) C Rmem(l3) /\
                                Robs(v) C Robs(l3) /\
                                Rlast(v) = {(0)} /\
                                Rhd(v) = Rsnd(l3)};