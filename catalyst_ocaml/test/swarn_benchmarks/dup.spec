relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rmem = Rhd*;

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};

relation Robs = Rob*;

relation Roas = Roa*;

relation Rfst (Pair(x, y)) = {(x)};

relation Rsnd (Pair(x, y)) = {(y)};

relation Rplmem (E) = {()}
                 | (LCons (p, pl)) = (Rfst (p) U Rsnd (p)) U (Rplmem (pl));

relation Rpairs (E) = {()} 
				| (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rpairs (pl));


relation Rplen  (E) = (0)
				| (LCons (p, pl)) = (1) + Rplen(pl);

relation Rlen (cons(x, xs)) = ((1) + Rlen(xs)) | nil = (0);


relation Rfla (E) = {()}
				| (L p) = (Rfst (p) X Rsnd (p))
				|  (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rfst(p) X Rplmem (pl)) U (Rsnd(p) X Rplmem (pl)) U Rfla (pl);

relation Rfstonly (E) = {()}
                  | (L p) = Rfst(p)
                  | (LCons (p, pl)) = Rfst(p) U Rfstonly(pl);


relation Rsndonly (E) = {()}
                  | (L p) = Rsnd(p)
                  | (LCons (p, pl)) = Rsnd(p) U Rsndonly(pl);

relation Rfstpairob (E) = {()}
                 | (L p) = {()}
                 | (LCons (p, pl)) = Rfst(p) X Rfstonly(pl);


relation Rsndpairob (E) = {()}
                 | (L p) = {()}
                 | (LCons (p, pl)) = Rsnd(p) X Rsndonly(pl);

relation Rfstpairobs = Rfstpairob*;

relation Rsndpairobs = Rsndpairob*;


dup : l1 -> {l | (Rmem(l1) = Rplmem(l)) /\ 
                                (Rplen(l) = Rlen(l1)) /\ 
                                (Rfstonly(l) = Rmem(l1)) /\
                                (Rsndonly(l) = Rmem(l1)) /\
                                (Rfstpairobs(l) = Robs(l1)) /\
                                (Rsndpairobs(l) = Robs(l1))};