# SKI

A simple system to explore combinatory logic and lambda calculus
inspired by the book [To Mock a Mockingbird][] by Raymond Smullyan.

## Description

This system is only meant to be used to play with combinatory logic
and lambda calculus. You can parse terms from strings with the usual
grammars into objects of the calculi and perform operations on them
such as reductions or translations from a system to another.
Additionally you may write simple programs for lambda calculus or
combinatory logic.

### Basic use

Load the system with ASDF and enter the `SKI` package:

```
CL-USER> (asdf:load-system :ski)
T
CL-USER> (in-package :ski)
#<PACKAGE "SKI">
SKI>
```

Parse the combinatory logic term **KI**xy, which is shorthand for
((**K I**) x) y. Here is its representation in the system:

```
SKI> (parse-combinator-term "KIxy")
#<COMBINATOR-APPLICATION
  (#<COMBINATOR-APPLICATION
    (#<COMBINATOR-APPLICATION
      (#<COMBINATOR (K) {1004F16C83}>
       #<COMBINATOR (I) {1004F3BC13}>)
     {100237FBB3}>
    #<COMBINATOR-VARIABLE (x) {100237FBE3}>)
   {100237FC13}>
   #<COMBINATOR-VARIABLE (y) {100237FC43}>)
{100237FC73}>
NIL
T
```

Now we reduce the term and sure enough the result is the variable y.
You probably know that combinatory logic doesn't have variables, I
just added them to have a clear feedback on the behavior of a
combinator. The variables are placeholders and don't have reduction
rules associated with them and just get shuffled around by the
combinators.

```
SKI> (reduce-term (parse-combinator-term "KIxy"))
#<COMBINATOR-VARIABLE (y) {100237FC43}>
SKI> (print-term *)
y
#<COMBINATOR-VARIABLE (y) {100237FC43}>
```

Some more examples:

```
SKI> (print-term (reduce-term (parse-combinator-term "STTx")))
xx
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (reduce-term (parse-combinator-term "B(B(B(TT)B)B)Txyz")))
zyx
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (make-combinator-application (get-combinator 'B) (make-combinator-variable 'x)))
Bx
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (lambda->ski (parse-lambda-term "λxy.yx")))
S(K(SI))K
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (reduce-term (lambda->ski (parse-lambda-term "(λxy.yx)ab"))))
ba
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (reduce-term (parse-lambda-term "(λmnf.m(nf))(λfx.f(f(fx)))(λfx.f(fx))")))
λf.λx.f(f(f(f(f(fx)))))
#<LAMBDA-ABSTRACTION ...>
SKI> (print-term (combinator->ski (get-combinator 'F)))
S(K(S(S(KS)(S(K(SI))K))))(S(KK)K)
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (combinator->ski (get-combinator 'U)))
S(K(SI))(SII)
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (combinator->lambda (get-combinator 'Ꙇ)))
λa.a(λa.λb.λc.ac(bc))(λa.λb.a)
#<LAMBDA-ABSTRACTION ...>
```

### REPL for combinators

A REPL for combinatory logic is also provided, in it you can use the
defined combinators, here is an example:

```
CL-USER> (asdf:load-system :ski)
T
CL-USER> (in-package :ski)
#<PACKAGE "SKI">
SKI> (combinator-driver-loop)
%%% Kxy
x
%%% M42
Parse error
%%% SB(Vx)zy
z(yxz)
%%% UBa
a(BBa)
%%% FMJBS
J(SS)
%%% W(EB)SKLM
SM
%%% Sxyz
xz(yz)
%%% BBBxywv
x(ywv)
%%% S(K(S(S(KS)(S(K(SI))K))))(S(KK)K)xyz
zyx
```

### REPL for lambda calculus

Similarly there is a REPL for lambda calculus which is invoked by the
[LAMBDA-DRIVER-LOOP][] function.

### Combinator programs

You can write combinator programs that define new combinators in term
of other combinators. Definitions of new combinators can take
parameters and can be recursive: this allows you to sidestep the need
for the fixed point principle which is used in the formal discussion
in the book. Defined combinators' names must be made up of uppercase
letters and start with a `@`. After the definitions (if any, they're
optional) you must provide some combinatory logic term to reduce. Here
is an example of arithmetic combinators:

```
@ZERO = I;                      # The number 0, as a numeral.
@ONE = V(KI)I;                  # The number 1, as a numeral.
@TWO = V(KI)(V(KI)I);           # The number 2, as a numeral.

@ISZERO = TK;                   # Test if a number is zero.
@SUCC = V(KI);                  # Compute the successor of a number.
@PRED = T(KI);                  # Compute the predecessor of a number.

# Add and multiply are combinators.
@ADD n m = (@ISZERO n)(m)(@SUCC (@ADD (@PRED n) m));
@MULT n m = (@ISZERO n)(@ZERO)(@ADD m (@MULT (@PRED n) m));

# Check the @ISZERO combinator.
@ISZERO @ZERO;
@ISZERO @ONE;
@ISZERO @TWO;

# Some arithmetic computations.
@SUCC @ONE;
@SUCC @TWO;
@SUCC (@SUCC (@SUCC @TWO));
@PRED (@SUCC (@SUCC (@SUCC @TWO)));
@ADD (@ADD @TWO (@SUCC (@SUCC @TWO))) @TWO;
@MULT (@ADD @TWO @TWO) (@SUCC @TWO);
```

Here is its output:

```
SKI> (run-combinator-program #p"programs/aritm.com")
K
KI
KI
V(KI)(V(KI)I)
V(KI)(V(KI)(V(KI)I))
V(KI)(V(KI)(V(KI)(V(KI)(V(KI)I))))
V(KI)(V(KI)(V(KI)(V(KI)I)))
V(KI)(V(KI)(V(KI)(V(KI)(V(KI)(V(KI)(V(KI)(V(KI)I)))))))
V(KI)(V(KI)(V(KI)(V(KI)(V(KI)(V(KI)(V(KI)(V(KI)(V(KI)(V(KI)(V(KI)(V(KI)I)))))))))))
#<COMBINATOR-APPLICATION ...>
```

### Lambda programs

In lambda programs you can define, in uppercase letters, names for
lambda terms and use them to build more complex lambda terms in
subsequent definitions and lambda terms. Definitions can't be
recursive, they're just a way to name a lambda term to make other
lambda terms more readable. After the definitions (if any, they're
optional) you must provide some lambda terms to reduce. Here is an
example of a lambda program that computes the factorial of 5:

```
ONE = λfx.fx;                                    # The number 1 as a Church numeral.
FIVE = λfx.f(f(f(f(fx))));                       # The number 5 as a Church numeral.

T = λxy.x;                                       # The True boolean value.
F = λxy.y;                                       # The False boolean value.
ISZERO = λn.n(T F)T;                             # Test if a number is zero.
PRED = λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u); # Compute the predecessor of a number.
MULT = λnm.λf.n(mf);                             # Compute the product of two numbers.

G = λf.λn.(ISZERO n)(ONE)(MULT n (f (PRED n)));  # Pre-factorial, used to build the factorial.
Y = (λxy.y(xxy)) (λxy.y(xxy));                   # Turing's fixed point combinator.
FACT = Y G;                                      # The factorial function.

FACT FIVE;                                       # The factorial of 5 as a Church numeral.
```

Here is its output, where we can see that the computed numeral is in
fact the representation of 120 as a Church numeral:

```
SKI> (run-lambda-program #p"programs/fact.lam")
λf.λx.f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(fx)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
#<LAMBDA-ABSTRACTION ...>
SKI> (church->natural *)
120 (7 bits, #x78, #o170, #b1111000)
```

### Command line interface

Run the `make` command to compile the system into an executable which
can run REPLs and evaluate programs.

## Installation

Clone this repository into a directory that ASDF and Quicklisp see
(most likely `~/quicklisp/local-projects/`) and then run
`(asdf:load-system :ski)` or `(ql:quickload :ski)` from your Lisp
REPL.

## Tests

Run `(asdf:test-system :ski)` from your Lisp REPL or `make test` from
the command line.

## Exercises

I brute-forced some exercises by generating all possible full binary
trees with a certain amount of leaves, then replacing the leaves with
combinators and reducing the obtained term to see if it satisfies the
exercise requirement. However, this approach is fundamentally flawed
because it often gets stuck in an infinite loop trying to reduce a
term that doesn't have a normal form.

## Interface

All symbols associated with combinators (`S`, `K`, `I`, `B`, `C`, `W`,
`M`, etc.) are exported.

### TERM
*Class*

### TERM-P
*Function*

### APPLICATION
*Class*

### APPLICATION-P
*Function*

### LEFT
*Generic Function*

### RIGHT
*Generic Function*

### MAKE-VARIABLE-NAME-GENERATOR
*Function*

### GENERATE-NAME
*Function*

### NAME
*Generic Function*

### VARIABLE
*Class*, *Generic Function*

### VARIABLE-P
*Function*

### SAME-VARIABLE-P
*Generic Function*

### OCCURS-FREE-P
*Generic Function*

### PRINT-TERM
*Generic Function*

### REDUCE-TERM
*Generic Function*

### TERM-EQUAL
*Generic Function*

### COMBINATOR-TERM
*Class*

### COMBINATOR-TERM-P
*Function*

### COMBINATOR
*Class*

### COMBINATOR-P
*Function*

### DEFINE-COMBINATOR
*Macro*

### GET-COMBINATOR
*Function*

### ARITY
*Generic Function*

### MAKE-COMBINATOR-APPLICATION
*Function*

### COMBINATOR-APPLICATION-P
*Function*

### MAKE-COMBINATOR-VARIABLE
*Function*

### COMBINATOR-VARIABLE-P
*Function*

### COMBINATOR-DRIVER-LOOP
*Function*

### LAMBDA-TERM
*Class*

### LAMBDA-TERM-P
*Function*

### MAKE-LAMBDA-ABSTRACTION
*Function*

### LAMBDA-ABSTRACTION-P
*Function*

### BODY
*Generic Function*

### MAKE-LAMBDA-APPLICATION
*Function*

### LAMBDA-APPLICATION-P
*Function*

### MAKE-LAMBDA-VARIABLE
*Function*

### LAMBDA-VARIABLE-P
*Function*

### FREE-VARIABLES
*Generic Function*

### BOUND-VARIABLES
*Generic Function*

### LAMBDA-COMBINATOR-P
*Generic Function*

### \*LAMBDA-REDUCTION-STRATEGY\*
*Dynamic Variable*

### SUBSTITUTE-AVOIDING-CAPTURE
*Generic Function*

### LAMBDA-DRIVER-LOOP
*Function*

### SK->GOEDEL
*Generic Function*

### GOEDEL->SK
*Function*

### NATURAL->CHURCH
*Function*

### CHURCH->NATURAL
*Function*

### NATURAL->BARENDREGT
*Function*

### BARENDREGT->NATURAL
*Function*

### COMBINATOR->LAMBDA
*Function*

### COMBINATOR->SKI
*Function*

### LAMBDA->SKI
*Generic Function*

### LAMBDA->SK
*Generic Function*

### PARSE-COMBINATOR-TERM
*Function*

### PARSE-LAMBDA-TERM
*Function*

### BUILD-LAMBDA-PROGRAM
*Function*

### RUN-LAMBDA-PROGRAM
*Function*

### RUN-COMBINATOR-PROGRAM
*Function*

[To Mock a Mockingbird]: https://en.wikipedia.org/wiki/To_Mock_a_Mockingbird
[LAMBDA-DRIVER-LOOP]: #LAMBDA-DRIVER-LOOP
