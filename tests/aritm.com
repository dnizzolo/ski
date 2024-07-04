# -*- mode: fundamental; -*-

@ZERO = I;
@ONE = V(KI)I;
@TWO = V(KI)(V(KI)I);

@ISZERO = TK;
@SUCC = V(KI);
@PRED = T(KI);

@ADD n m = (@ISZERO n)(m)(@SUCC (@ADD (@PRED n) m));
@MULT n m = (@ISZERO n)(@ZERO)(@ADD m (@MULT (@PRED n) m));

@MULT (@ADD @TWO @TWO) (@SUCC @TWO);