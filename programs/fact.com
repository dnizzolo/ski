# -*- mode: fundamental; -*-

zero = I;
one = V(KI)I;
five = V(KI)(V(KI)(V(KI)(V(KI)(V(KI)I))));

iszero = TK;
succ = V(KI);
pred = T(KI);

add n m = (iszero n)(m)(succ (add (pred n) m));
mult n m = (iszero n)(zero)(add m (mult (pred n) m));

fact n = (iszero n)(one)(mult n (fact (pred n)));

fact five;