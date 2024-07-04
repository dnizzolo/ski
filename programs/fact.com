# -*- mode: fundamental; -*-

@ZERO = I;
@ONE = V(KI)I;
@FIVE = V(KI)(V(KI)(V(KI)(V(KI)(V(KI)I))));

@ISZERO = TK;
@SUCC = V(KI);
@PRED = T(KI);

@ADD n m = (@ISZERO n)(m)(@SUCC (@ADD (@PRED n) m));
@MULT n m = (@ISZERO n)(@ZERO)(@ADD m (@MULT (@PRED n) m));

@FACT n = (@ISZERO n)(@ONE)(@MULT n (@FACT (@PRED n)));

@FACT @FIVE;