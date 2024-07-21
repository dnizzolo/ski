# SKI

A simple system to explore combinators inspired by the book [To Mock a
Mockingbird](https://en.wikipedia.org/wiki/To_Mock_a_Mockingbird) by
Raymond Smullyan.

## Description

This system is only meant to be used to play with combinatory logic
and lambda calculus. You can parse terms from strings with the usual
grammars and perform operations on them such as reduction or
translations from a system to another. Additionally you may write
simple programs for lambda calculus or combinatory logic.

## Examples

### General use

Load the system with ASDF and enter the `SKI` package:

```
CL-USER> (asdf:load-system :ski)
T
CL-USER> (in-package :ski)
#<PACKAGE "SKI">
SKI> (parse-combinator-term "KIxy")
#<COMBINATOR-APPLICATION (#<COMBINATOR-APPLICATION (#<COMBINATOR-APPLICATION (#<COMBINATOR (K) {1004F16C83}> #<COMBINATOR (I) {1004F3BC13}>) {100237FBB3}> #<COMBINATOR-VARIABLE (x) {100237FBE3}>) {100237FC13}> #<COMBINATOR-VARIABLE (y) {100237FC43}>) {100237FC73}>
NIL
T
SKI> (reduce-term *)
#<COMBINATOR-VARIABLE (y) {100237FC43}>
SKI> (print-term *)
y
#<COMBINATOR-VARIABLE (y) {100237FC43}>
SKI> (print-term (reduce-term (parse-combinator-term "STTx")))
xx
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (reduce-term (parse-combinator-term "B(B(B(TT)B)B)Txyz")))
zyx
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
```

### REPL for combinators

A REPL for combinatory logic is also provided, in it you can use the
defined combinators.

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

Similarly there is a REPL for lambda calculus which is invoked with
the `lambda-driver-loop` function.

### Combinator programs

You can write combinator programs that define new combinators in term
of other combinators. Definitions of new combinators can take
parameters and can be recursive: this allows you to sidestep the need
for the fixed point principle which they formally use in the book.
Defined combinators names must be made up of uppercase letters and
start with a `@`. After the definitions (if any) you must provide some
combinatory logic term to reduce.

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

### Lambda programs

In lambda programs you can define, in uppercase letters, names for
lambda terms and use them to build more complex lambda terms in
subsequent definitions and lambda terms. Definitions can't be
recursive, they're just a way to name a lambda term to make other
lambda terms more readable. After the definitions (if any) you must
provide some lambda terms to reduce. Here is an example of a lambda
program that computes the factorial of 5.

```
ONE = λfx.fx;                                    # The number 1 as a Church numeral.
FIVE = λfx.f(f(f(f(fx))));                       # The number 5 as a Church numeral.

T = λxy.x;                                       # The True boolean value.
F = λxy.y;                                       # The False boolean value.
ISZERO = λn.n(T F)T;                             # Test if a number is zero.
PRED = λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u); # Return the predecessor of a number.
MULT = λnm.λf.n(mf);                             # Multiply two numbers.

G = λf.λn.(ISZERO n)(ONE)(MULT n (f (PRED n)));  # Pre-factorial, used to build the factorial.
Y = (λxy.y(xxy)) (λxy.y(xxy));                   # Turing's fixed point combinator.
FACT = Y G;                                      # The factorial function.

FACT FIVE;                                       # The factorial of 5 as a Church numeral.
```

### CLI

Run the `make` command to compile the system into an executable which
can run REPLs and evaluate programs.

## Exports

* `term` - base class for terms.
* `term-p` - check if an object is a term.
* `variable` - base class for variables and generic function that
  returns the variable of a lambda abstraction.
* `variable-p` - check if an object is a variable.
* `name` - get the name of an object that has a name slot.
* `same-variable-p` - check if two variable have the same name.
* `make-variable-name-generator` - make an object that generates
  variable names.
* `generate-name` - generate the next variable name from a generator
  object.
* `application` - base class for applications.
* `application-p` - check if an object is an application.
* `left` - get the left object of an application, i.e. the function.
* `right` - get the right object of an application, i.e. the argument.
* `occurs-free-p` - check if a variable occurs free in a term.
* `print-term` - print a parsable representation of a term.
* `term-equal` - check if two terms are equal.
* `reduce-term` - reduce a term to its normal form.
* `combinator-driver-loop` - a REPL for combinatory logic.
* `combinator-term` - base class for combinatory logic terms.
* `combinator-term-p` - check if an object is a combinatory logic
  term.
* `combinator` - class for a combinator in combinatory logic.
* `combinator-p` - check if an object is a combinator.
* `arity` - return the number of "arguments" a term takes.
* `make-combinator-variable` - make a variable for combinatory logic.
* `combinator-variable-p` - check if an object is a combinatory logic
  variable.
* `make-combinator-application` - make a combinatory logic
  application.
* `combinator-application-p` - check if an object is an application in
  combinatory logic.
* `define-combinator` - define a combinator.
* `get-combinator` - get the combinator object associated with a
  symbol.
* `lambda-term` - base class for lambda calculus terms.
* `lambda-term-p` - check if an object is a lambda calculus term.
* `make-lambda-variable` - make a variable for lambda calculus.
* `lambda-variable-p` - check if an object is a lambda calculus
  variable.
* `make-lambda-application` - make a lambda-calculus application.
* `lambda-application-p` - check if an object is a lambda calculus
  application.
* `make-lambda-abstraction` - make a lambda calculus abstraction.
* `lambda-abstraction-p` - check if an object is a lambda calculus
  abstraction.
* `body` - get the body of a lambda calculus abstraction.
* `free-variables` - return the free variables in a lambda calculus
  term.
* `bound-variables` - return the bound variables in a lambda calculus
  term.
* `substitute-avoiding-capture` - substitute without changing the
  term's meaning.
* `lambda-combinator-p` - check if a lambda term is a combinator.
* `lambda-driver-loop` - a REPL for lambda calculus.
* `sk->goedel` - return the Gödel number of a SK calculus term.
* `goedel->sk` - return the SK calculus term of a Gödel number.
* `natural->church` - return the Church numeral of a natural number.
* `church->natural` - return the natural number represented by a
  Church numeral.
* `natural->barendregt` - return the numeral of a natural number
  according to the encoding used in the book.
* `barendregt->natural` - return the natural number represented by a
  numeral according to the encoding used in the book.
* `combinator->ski` - express a combinator using only the S, K, and I
  combinators.
* `lambda->ski` - traduce a term from lambda calculus to SKI calculus.
* `lambda->sk` - traduce a term from lambda calculus to SK calculus.
* `parse-combinator-term` - parse a combinatory logic term from a
  string.
* `parse-lambda-term` - parse a lambda calculus term from a string.
* `build-lambda-program` - given a lambda program, return the list of
  pure lambda calculus terms defined by the program, ready to be
  evaluated.
* `run-lambda-program` - run a lambda program.
* `run-combinator-program` - run a combinator program.

All symbols associated with combinators are exported.

## Exercises

I brute-forced some exercises by generating all possible full binary
trees with a certain amount of leaves, then replacing the leaves with
combinators and reducing the obtained term to see if it satisfies the
exercise requirement. This approach doesn't usually work however
because it often gets stuck in an infinite loop trying to reduce a
term that doesn't have a normal form.

## Tests

Run `(asdf:test-system :ski)`.
