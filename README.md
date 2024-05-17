# SKI

A simple system to explore combinators inspired by the book [To Mock a
Mockingbird](https://en.wikipedia.org/wiki/To_Mock_a_Mockingbird) by
Raymond Smullyan.

## Quick examples

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

## Exports

* `term` - base class for terms.
* `term-p` - check if an object is a term.
* `variable` - base class for variables.
* `variable-p` - check if an object is a variable.
* `variable-name` - get the name of a variable.
* `same-variable-p` - check if two variable have the same name.
* `make-variable-name-generator` - make an object that generates variable names.
* `generate-name` - generate the next variable name from a generator object.
* `application` - base class for applications.
* `application-p` - check if an object is an application.
* `application-left` - get the left object of an application, i.e. the "function".
* `application-right` - get the right object of an application, i.e. the "argument".
* `occurs-free-p` - check if a variable occurs free in a term.
* `print-term` - print a parsable representation of a term.
* `term-equal` - check if two terms are equal.
* `reduce-term` - reduce a term to its beta normal form.
* `combinator-term` - base class for combinatory logic terms.
* `combinator-term-p` - check if an object is a combinatory logic term.
* `combinator` - class for a combinator in combinatory logic.
* `combinator-p` - check if an object is a combinator.
* `combinator-name` - get the name of a combinator.
* `combinator-arity` - return the number of "arguments" a combinator takes.
* `make-combinator-variable` - make a variable for combinatory logic.
* `combinator-application-p` - check if an object is an application of combinatory logic.
* `define-combinator` - define a combinator.
* `get-combinator` - get the combinator object associated with a symbol.
* `lambda-term` - base class for lambda calculus terms.
* `lambda-term-p` - check if an object is a lambda calculus term.
* `make-lambda-variable` - make a variable for lambda calculus.
* `lambda-variable-p` - check if an object is a lambda calculus variable.
* `make-lambda-application` - make a lambda-calculus application.
* `lambda-application-p` - check if an object is a lambda calculus application.
* `make-lambda-abstraction` - make a lambda calculus abstraction.
* `lambda-abstraction-p` - check if an object is a lambda calculus abstraction.
* `lambda-abstraction-variable` - get the variable of a lambda calculus abstraction.
* `lambda-abstraction-body` - get the body of a lambda calculus abstraction.
* `free-variables` - return the free variables in a lambda calculus term.
* `bound-variables` - return the bound variables in a lambda calculus term.
* `substitute-avoiding-capture` - substitute without changing the term's meaning.
* `churchify` - return the Church numeral of a natural number.
* `dechurchify` - return the natural number represented by a Church numeral.
* `combinator->ski` - express a combinator using only the S, K, and I combinators.
* `lambda->ski` - traduce a term from lambda calculus to SKI calculus.
* `lambda->sk` - traduce a term from lambda calculus to SK calculus.
* `parse-combinator-term` - parse a combinatory logic term from a string.
* `parse-lambda-term` - parse a lambda calculus term from a string.

All symbols associated with combinators are exported.

## Tests

Run `(asdf:test-system :ski)`.
