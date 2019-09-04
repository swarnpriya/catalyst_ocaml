relation Rhd (cons(x,xs)) = {(x)} | nil = {()};

relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};

relation Robs = Rob*;

relation Rfst (Pair (x,y)) = {(x)};

relation Rsnd (Pair (x,y)) = {(y)};

relation Rmem = Rhd*;

relation Rplmem (E) = {()} 
				| (L p) =  Rfst (p) U Rsnd (p) 
				| (LCons (p, pl)) = (Rfst (p) U Rsnd (p)) U (Rplmem (pl));

relation Rpairs (E) = {()} 
				| (L p) =  (Rfst (p) X Rsnd (p)) 
				| (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rpairs (pl));

relation Rfla (E) = {()}
				| (L p) = (Rfst (p) X Rsnd (p))
				|  (LCons (p, pl)) = (Rfst (p) X Rsnd (p)) U (Rfst(p) X Rplmem (pl)) U (Rsnd(p) X Rplmem (pl)) U Rfla (pl);


relation Rnext_hd nil = {()}
                  | (cons (x, xs)) = {(Rhd(xs))};

relation Rnext_next_hd (nil) = {()}
                  | (cons (x, xs) = {(Rnext_hd(xs))};

reg_or : e -> e' -> e'' -> l1 -> {l | Rmem(l) = Rmem(l1) /\
                                      Rhd(l) = {(e)} /\
                                      (Rnext_hd(l) = {(e')} \/ Rnext_hd(l) = {(e'')})}