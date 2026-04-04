# SKI

A simple system to explore combinatory logic and lambda calculus
inspired by the book [To Mock a Mockingbird][] by Raymond Smullyan.

## Description

This system is only meant to be used to play with combinatory logic
and lambda calculus. You can parse terms from strings with the usual
grammars into objects of the calculi and perform operations on them
such as reductions or translations from a system to another.

### Basic use

Load the system with ASDF and enter the `SKI` package:

```
CL-USER> (asdf:load-system :ski)
T
CL-USER> (in-package #:ski)
#<PACKAGE "SKI">
SKI>
```

Parse the combinatory logic term **KI**x y, which is shorthand for
((**K I**) x) y. Here is its representation in the system:

```
SKI> (parse-combinator-term "KIx y")
#<COMBINATOR-APPLICATION
  (#<COMBINATOR-APPLICATION
     (#<COMBINATOR-APPLICATION
        (#<COMBINATOR K/2 {1004F16C83}>
         #<COMBINATOR I/1 {1004F3BC13}>)
      {100237FBB3}>
     #<COMBINATOR-VARIABLE x {100237FBE3}>)
   {100237FC13}>
  #<COMBINATOR-VARIABLE y {100237FC43}>)
{100237FC73}>
NIL
T
```

Now we reduce the term and sure enough the result is the variable y.
You probably know that combinatory logic doesn't have variables, I
just added them to have a clear visualization of the behavior of a
combinator. The variables are placeholders and don't have reduction
rules associated with them and just get shuffled around by the
combinators.

```
SKI> (reduce-term (parse-combinator-term "KI x y"))
#<COMBINATOR-VARIABLE y {100237FC43}>
SKI> (print-term *)
y
#<COMBINATOR-VARIABLE y {100237FC43}>
```

Some more examples:

```
SKI> (print-term (reduce-term (parse-combinator-term "I thing")))
thing
#<COMBINATOR-VARIABLE thing {120387C033}>
SKI> (print-term (reduce-term (parse-combinator-term "STT x")))
x x
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (reduce-term (parse-combinator-term "B(B(B(TT)B)B)Tx y z")))
z y x
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (make-combinator-application (get-combinator 'B) (make-combinator-variable "x")))
B x
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (lambda->ski (parse-lambda-term "λx y.y x")))
S(K(S I)) K
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (reduce-term (lambda->ski (parse-lambda-term "(λx y.y x)a b"))))
b a
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (reduce-term (parse-lambda-term "(λm n f.m(n f))(λf x.f(f(f x)))(λf x.f(f x))")))
λf.λx.f(f(f(f(f(f x)))))
#<LAMBDA-ABSTRACTION ...>
SKI> (print-term (combinator->ski (get-combinator 'F)))
S(K(S(S(K S)(S(K(S I)) K))))(S(K K) K)
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (combinator->ski (get-combinator 'U)))
S(K(S I))(S I I)
#<COMBINATOR-APPLICATION ...>
SKI> (print-term (combinator->lambda (get-combinator 'Ꙇ)))
λa.a (λa.λb.λc.a c (b c)) (λa.λb.a)
#<LAMBDA-ABSTRACTION ...>
```

### REPL

Start a REPL with the functions [RUN-COMBINATOR-REPL][] and
[RUN-LAMBDA-REPL][] in which one can enter terms to be reduced and
also introduce bindings. The REPLs support commands (they start with a
colon like Common Lisp keywords) that load bindings from a file, write
current bindings to a file, list the currently loaded bindings and so
on.

```
SKI> (run-combinator-repl)
>>> :print-commands
:load-bindings-file <file>
:write-bindings-file <file>
:print-bindings
:delete-binding <name>
:print-commands
```

### REPL for combinators

A REPL for combinatory logic is available, in it you can use the
defined combinators and define you own (recursive) bindings: they bind
variables to combinatory logic terms.

```
SKI> (run-combinator-repl)
>>> K x y
x
>>> M42
Unexpected character '4'.
At line 1, column 2:
M42
 ^
>>> SB(Vx)z y
z(y x z)
>>> UBa
a(B B a)
>>> FMJBS
J(S S)
>>> W(EB)SKLM
S M
>>> S x y z
x z(y z)
>>> BBBx y w v
x(y w v)
>>> S(K(S(S(KS)(S(K(SI))K))))(S(KK)K)x y z
z y x
>>> zero = I
Created binding zero.
>>> succ = V(KI)
Created binding succ.
>>> one = succ zero
Created binding one.
>>> one
V(K I) I
>>> pred = T(KI)
Created binding pred.
>>> iszero = TK
Created binding iszero.
>>> add n m = (iszero n)(m)(succ (add (pred n) m))
Created binding add.
>>> mult n m = (iszero n)(zero)(add m (mult (pred n) m))
Created binding mult.
>>> add (succ (succ one)) (mult (mult (succ one) (succ (succ one))) (succ (succ (succ one))))
V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I) I))))))))))))))))))))))))))
```

### REPL for lambda calculus

Similarly there is a REPL for lambda calculus which is invoked by the
[RUN-LAMBDA-REPL][] function. It allows bindings that are not recursive.
In order to achieve recursion one must use a fixed point combinator,
like the Y combinator.

### Combinator programs

A combinator program is just a file that contains combinator terms and
bindings. To run a combinator program, an environment that contains
all the bindings in the file is created and then the terms are reduced
and printed in the order they appear in the file. The result of the
last reduction is returned.

```
zero = I;                      # The number 0, as a numeral.
one = V(KI)I;                  # The number 1, as a numeral.
two = V(KI)(V(KI)I);           # The number 2, as a numeral.

iszero = TK;                   # Test if a number is zero.
succ = V(KI);                  # Compute the successor of a number.
pred = T(KI);                  # Compute the predecessor of a number.

# Add and multiply are combinators.
add n m = (iszer n)(m)(succ (add (pred n) m));
mult n m = (iszero n)(zero)(add m (mult (pred n) m));

# Check the iszero combinator.
iszero zero;
iszero one;
iszero two;

# Some arithmetic computations.
succ one;
succ two;
succ (succ (succ two));
pred (succ (succ (succ two)));
add (add two (succ (succ two))) two;
mult (add two two) (succ two);
```

Here is its output:

```
SKI> (run-combinator-program #p"programs/arithm.com")
K
K I
K I
V(K I)(V(K I) I)
V(K I)(V(K I)(V(K I) I))
V(K I)(V(K I)(V(K I)(V(K I)(V(K I) I))))
V(K I)(V(K I)(V(K I)(V(K I) I)))
V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I) I)))))))
V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I)(V(K I) I)))))))))))
#<COMBINATOR-APPLICATION ...>
```

### Lambda programs

Similar to combinator programs, a lambda program is a collection of
terms and bindings. In lambda programs you can define bindings for
lambda terms and use them to build more complex lambda terms.
Definitions can't be recursive, they're just a way to name a lambda
term to make other lambda terms more readable. Order of definition for
lambda bindings matters. Here is an example of a lambda program that
computes the factorial of 5:

```
one = λf x.f x;                                    # The number 1 as a Church numeral.
five = λf x.f(f(f(f(f x))));                       # The number 5 as a Church numeral.

t = λx y.x;                                        # The True boolean value.
f = λx y.y;                                        # The False boolean value.
iszero = λn.n(t f)t;                               # Test if a number is zero.
pred = λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u);   # Compute the predecessor of a number.
mult = λn m.λf.n(m f);                             # Compute the product of two numbers.

g = λf.λn.(iszero n)(one)(mult n (f (pred n)));    # Pre-factorial, used to build the factorial.
u = (λx y.y(x x y)) (λx y.y(x x y));               # Turing's fixed point combinator.
fact = u g;                                        # The factorial function.

fact five;                                         # The factorial of 5 as a Church numeral.
```

Here is its output, where we can see that the computed numeral is in
fact the representation of 120 as a Church numeral:

```
SKI> (run-lambda-program #p"programs/fact.lam")
λf.λx.f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f x)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
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

### RUN-LAMBDA-PROGRAM
*Function*

### RUN-COMBINATOR-PROGRAM
*Function*

### RUN-LAMBDA-REPL
*Function*

### RUN-COMBINATOR-REPL
*Function*

[To Mock a Mockingbird]: https://en.wikipedia.org/wiki/To_Mock_a_Mockingbird
[RUN-COMBINATOR-REPL]: #RUN-COMBINATOR-REPL
[RUN-LAMBDA-REPL]: #RUN-LAMBDA-REPL
