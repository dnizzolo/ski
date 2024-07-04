# -*- mode: fundamental; -*-

@ZERO = I;
@ONE = V(KI)I;
@TWO = V(KI)(V(KI)I);

@ISZERO = TK;
@SUCC = V(KI);
@PRED = T(KI);

@ADD n m = (@ISZERO n)(m)(@SUCC (@ADD (@PRED n) m));
@MULT n m = (@ISZERO n)(@ZERO)(@ADD m (@MULT (@PRED n) m));

@ISZERO @ZERO;
@ISZERO @ONE;
@ISZERO @TWO;

@SUCC @ONE;
@SUCC @TWO;
@SUCC (@SUCC (@SUCC @TWO));
@PRED (@SUCC (@SUCC (@SUCC @TWO)));
@ADD (@ADD @TWO (@SUCC (@SUCC @TWO))) @TWO;
@MULT (@ADD @TWO @TWO) (@SUCC @TWO);