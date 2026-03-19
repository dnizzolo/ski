# -*- mode: fundamental; -*-

zero = I;
one = V(KI)I;
two = V(KI)(V(KI)I);

iszero = TK;
succ = V(KI);
pred = T(KI);

add n m = (iszero n)(m)(succ (add (pred n) m));
mult n m = (iszero n)(zero)(add m (mult (pred n) m));

iszero zero;
iszero one;
iszero two;

succ one;
succ two;
succ (succ (succ two));
pred (succ (succ (succ two)));
add (add two (succ (succ two))) two;
mult (add two two) (succ two);
