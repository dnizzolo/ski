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
