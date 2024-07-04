(defpackage #:ski
  (:use #:cl)
  (:shadow #:variable)
  (:export
   #:term
   #:term-p
   #:variable
   #:name
   #:variable-p
   #:same-variable-p
   #:make-variable-name-generator
   #:generate-name
   #:application
   #:left
   #:right
   #:application-p
   #:occurs-free-p
   #:print-term
   #:term-equal
   #:reduce-term
   #:driver-loop
   #:combinator-term
   #:combinator-term-p
   #:combinator
   #:arity
   #:combinator-p
   #:make-combinator-variable
   #:combinator-variable-p
   #:make-combinator-application
   #:combinator-application-p
   #:define-combinator
   #:get-combinator
   #:lambda-term
   #:lambda-term-p
   #:make-lambda-variable
   #:lambda-variable-p
   #:make-lambda-application
   #:lambda-application-p
   #:make-lambda-abstraction
   #:lambda-abstraction-p
   #:body
   #:free-variables
   #:bound-variables
   #:substitute-avoiding-capture
   #:lambda-combinator-p
   #:sk->goedel
   #:goedel->sk
   #:natural->church
   #:church->natural
   #:natural->barendregt
   #:barendregt->natural
   #:combinator->ski
   #:lambda->ski
   #:lambda->sk
   #:parse-combinator-term
   #:parse-lambda-term
   #:build-lambda-program
   #:run-lambda-program
   #:run-combinator-program
   #:S
   #:S1
   #:K
   #:K2
   #:I
   #:I2
   #:B
   #:B1
   #:B2
   #:B3
   #:C
   #:C*
   #:C**
   #:D
   #:D1
   #:D2
   #:E
   #:Ê
   #:F
   #:F*
   #:F**
   #:G
   #:G1
   #:G2
   #:H
   #:H*
   #:J
   #:J1
   #:L
   #:M
   #:M2
   #:O
   #:Q
   #:Q1
   #:Q2
   #:Q3
   #:Q4
   #:R
   #:R*
   #:R**
   #:Θ
   #:T
   #:U
   #:V
   #:V*
   #:V**
   #:W
   #:W1
   #:W*
   #:W**
   #:Φ
   #:Ψ
   #:Γ))
