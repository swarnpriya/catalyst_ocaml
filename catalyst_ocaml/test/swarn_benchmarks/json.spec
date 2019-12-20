relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};

relation Robs = Rob*;

relation Roas = Roa*;

relation Rfst (Pair(x, y)) = {(x)};

relation Rsnd (Pair(x, y)) = {(y)};

relation Robsfst (Pair (x, y)) = Robs(x);

relation Robssnd (Pair (x, y)) = Robs(y);

relation Rplmem (E) = {()}
                 | (LCons (p, pl)) = (Rfst (p) U Rsnd (p)) U (Rplmem (pl));

relation Rpairs (E) = {()} 
        | (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rpairs (pl));


relation Rplen  (E) = (0)
        | (LCons (p, pl)) = (1) + Rplen(pl);

relation Rlen (cons(x, xs)) = ((1) + Rlen(xs)) | nil = (0);


relation Rfla (E) = {()}
        |  (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rfst(p) X Rplmem (pl)) U (Rsnd(p) X Rplmem (pl)) U Rfla (pl);

relation Rfstonly (E) = {()}
                  | (LCons (p, pl)) = Rfst(p) U Rfstonly(pl);


relation Rsndonly (E) = {()}
                  | (LCons (p, pl)) = Rsnd(p) U Rsndonly(pl);

relation Rfstpairob (E) = {()}
                 | (LCons (p, pl)) = Rfst(p) X Rfstonly(pl);


relation Rsndpairob (E) = {()}
                 | (LCons (p, pl)) = Rsnd(p) X Rsndonly(pl);

relation Rfstpairobs = Rfstpairob*;

relation Rsndpairobs = Rsndpairob*;

json_parse : j -> {j' | Rplem(j') = Rplem (j) /\
                        Rplen(j') = Rplen (j) /\
                        Rfstonly(j') = Rfstonly (j) /\
                        Rsndonly(j') = Rsndonly (j)};

access_by_field : j -> f -> {v | {(v)} C= Rsndonly(j) /\ 
                                 {(f)} C= Rfstonly(j) /\
                                 {(f,v)} C= Rpairs(j)};

modify_by_field : j -> f -> v -> {j' | {(f)} C= Rfstonly(j) /\ 
                                       {(f)} C= Rfstonly(j') /\ 
                                       {(f,v)} C= Rpairs(j')};

delete_by_field : j -> f -> {j' | {(f)} C= Rfstonly(j) /\ 
                                  not ({(f)} C= Rfstonly(j'))};