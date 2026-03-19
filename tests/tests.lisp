(defpackage #:ski/test
  (:use #:cl #:ski #:parachute)
  (:shadowing-import-from #:ski #:variable)
  (:shadowing-import-from #:parachute #:body)
  (:shadowing-import-from #:parachute #:name))

(in-package #:ski/test)

(define-test parse-combinator-term-test
  (is term-equal
      (get-combinator 'W**)
      (parse-combinator-term "W**"))
  (is term-equal
      (make-combinator-application
       (make-combinator-application
        (get-combinator 'S)
        (get-combinator 'O))
       (make-combinator-variable #\p))
      (parse-combinator-term "SOp"))
  (is term-equal
      (make-combinator-application
       (make-combinator-application
        (get-combinator 'S)
        (get-combinator 'S))
       (get-combinator 'K))
      (parse-combinator-term "SSK"))
  (is term-equal
      (make-combinator-application
       (make-combinator-application
        (make-combinator-application
         (make-combinator-variable #\x)
         (make-combinator-variable #\y))
        (make-combinator-application
         (make-combinator-application
          (make-combinator-variable #\z)
          (make-combinator-variable #\w))
         (make-combinator-variable #\y)))
       (make-combinator-application
        (make-combinator-variable #\x)
        (make-combinator-variable #\z)))
      (parse-combinator-term "x y(z w y)(x z)"))
  (is term-equal
      (make-combinator-application
       (make-combinator-application
        (make-combinator-application
         (make-combinator-application
          (make-combinator-application
           (get-combinator 'S)
           (get-combinator 'B))
          (get-combinator 'C))
         (get-combinator 'W))
        (make-combinator-application
         (get-combinator 'M)
         (get-combinator 'L)))
       (get-combinator 'T))
      (parse-combinator-term "SBCW(ML)T")))

(define-test parse-lambda-term-test
  (is term-equal
      (make-lambda-application
       (make-lambda-variable #\x)
       (make-lambda-variable #\y))
      (parse-lambda-term "x y"))
  (is term-equal
      (make-lambda-application
       (make-lambda-application
        (make-lambda-variable #\v)
        (make-lambda-application
         (make-lambda-variable #\w)
         (make-lambda-variable #\w)))
       (make-lambda-variable #\u))
      (parse-lambda-term "v(w w)u"))
  (is term-equal
      (make-lambda-abstraction
       (make-lambda-variable #\x)
       (make-lambda-variable #\x))
      (parse-lambda-term "λx.x"))
  (is term-equal
      (make-lambda-application
       (make-lambda-abstraction
        (make-lambda-variable #\x)
        (make-lambda-variable #\x))
       (make-lambda-variable #\z))
      (parse-lambda-term "(λx.x)z"))
  (is term-equal
      (make-lambda-application
       (make-lambda-application
        (make-lambda-abstraction
         (make-lambda-variable #\x)
         (make-lambda-variable #\x))
        (make-lambda-abstraction
         (make-lambda-variable #\f)
         (make-lambda-abstraction
          (make-lambda-variable #\y)
          (make-lambda-application
           (make-lambda-application
            (make-lambda-variable #\f)
            (make-lambda-variable #\y))
           (make-lambda-variable #\y)))))
       (make-lambda-variable #\m))
      (parse-lambda-term "(λx.x)(λf y.f y y)m")))

(define-test combinator-reduction-test
  :depends-on (parse-combinator-term-test)
  (macrolet ((is-reduced (expected term)
               `(is term-equal
                    (parse-combinator-term ,expected)
                    (reduce-term (parse-combinator-term ,term)))))
    (is-reduced "I" "SKI(KIS)")
    (is-reduced "S" "KS(I(SKSI))")
    (is-reduced "K" "SKIK")
    (is-reduced "SKI" "SKISKISKISKISKISKI")
    (is-reduced "SSI" "SKISKISKSKKIISSISKKSISISISIKSSKISSI")
    (is-reduced "S" "SSKSKSKSIISKSI(SSIKS)")
    (is-reduced "SKS" "KSKSISSKISKISKSISKISKISKISKISKISKISKISKISKI(KISSISKSISKI(ISKISK)SKISK)SIKS")
    (is-reduced "a(b c)" "B a b c")
    (is-reduced "a(b c)" "S(KS)K a b c")
    (is-reduced "a(b b)" "S(K a)(SII)b")
    (is-reduced "a" "V a b K")
    (is-reduced "b" "V a b (KI)")
    (is-reduced "x x" "Mx")
    (is-reduced "x x" "STTx")
    (is-reduced "xII" "I2x")
    (is-reduced "x z(y z)" "C(Q(QQ(QQ))Q)W x y z")
    (is-reduced "x z(y z)" "C(QQ(Q(QQ)Q))W x y z")))

(define-test lambda->ski-test
  :depends-on (parse-combinator-term-test parse-lambda-term-test)
  (is term-equal
      (parse-combinator-term "S(K(SI))K")
      (lambda->ski (parse-lambda-term "λx y.y x")))
  (is term-equal
      (make-combinator-application
       (make-combinator-variable #\b)
       (make-combinator-variable #\a))
      (reduce-term (lambda->ski (parse-lambda-term "(λx y.y x)a b"))))
  (is term-equal
      (get-combinator 'K)
      (reduce-term (lambda->ski (parse-lambda-term "(λn.n(λx.λx y.y)(λx y.x))λf x.x"))))
  (is term-equal
      (make-combinator-application
       (get-combinator 'K)
       (get-combinator 'I))
      (reduce-term (lambda->ski (parse-lambda-term "(λn.n(λx.λx y.y)(λx y.x))λf x.f x")))))

(define-test lambda->sk-test
  :depends-on (parse-combinator-term-test parse-lambda-term-test)
  (is term-equal
      (parse-combinator-term "S(K(S(SKK)))K")
      (lambda->sk (parse-lambda-term "λx y.y x")))
  (is term-equal
      (make-combinator-application
       (make-combinator-variable #\b)
       (make-combinator-variable #\a))
      (reduce-term (lambda->sk (parse-lambda-term "(λx y.y x)a b"))))
  (is term-equal
      (get-combinator 'K)
      (reduce-term (lambda->sk (parse-lambda-term "(λn.n(λx.λx y.y)(λx y.x))λf x.x"))))
  (is term-equal
      (parse-combinator-term "K(SKK)")
      (reduce-term (lambda->sk (parse-lambda-term "(λn.n(λx.λx y.y)(λx y.x))λf x.f x")))))

(define-test combinator->lambda-test
  :depends-on (combinator-reduction-test parse-lambda-term-test lambda-equality-test)
  (is term-equal
      (combinator->lambda (get-combinator 'M))
      (parse-lambda-term "λx.x x"))
  (is term-equal
      (combinator->lambda (get-combinator 'B))
      (parse-lambda-term "λx y z.x(y z)"))
  (is term-equal
      (combinator->lambda (get-combinator 'L))
      (parse-lambda-term "λx y.x(y y)"))
  (is term-equal
      (combinator->lambda (get-combinator 'W))
      (parse-lambda-term "λx y.x y y"))
  (is term-equal
      (combinator->lambda (get-combinator 'K))
      (parse-lambda-term "λx y.x"))
  (is term-equal
      (combinator->lambda (get-combinator 'I))
      (parse-lambda-term "λx.x"))
  (is term-equal
      (combinator->lambda (get-combinator 'S))
      (parse-lambda-term "λx y z.x z(y z)")))

(define-test combinator->ski-test
  :depends-on (combinator-reduction-test)
  (macrolet ((is-ski (combinator)
               (let ((collect-variables-form
                       `(loop with g = (make-variable-name-generator)
                              with arity = (arity (get-combinator ,combinator))
                              repeat arity
                              collect (make-combinator-variable (generate-name g)))))
                 `(is term-equal
                      (reduce-term
                       (reduce #'make-combinator-application
                               ,collect-variables-form
                               :initial-value (get-combinator ,combinator)))
                      (reduce-term
                       (reduce #'make-combinator-application
                               ,collect-variables-form
                               :initial-value (combinator->ski
                                               (get-combinator ,combinator))))))))
    (is-ski 'B)
    (is-ski 'C)
    (is-ski 'W)
    (is-ski 'U)
    (is-ski 'G)
    (is-ski 'T)
    (is-ski 'E)
    (is-ski 'Q)
    (is-ski 'M)
    (is-ski 'S)
    (is-ski 'K)
    (is-ski 'I)
    (is-ski 'O)
    (is-ski 'L)
    (is-ski 'F)))

(define-test lambda-equality-test
  :depends-on (parse-lambda-term-test)
  (macrolet ((expect-lambda-equal (comp term1 term2)
               `(,comp (term-equal (parse-lambda-term ,term1)
                                   (parse-lambda-term ,term2)))))
    (expect-lambda-equal true "λx.x" "λx.x")
    (expect-lambda-equal true "λx.a" "λy.a")
    (expect-lambda-equal true "b" "b")
    (expect-lambda-equal false "a" "b")
    (expect-lambda-equal false "λx.x" "λy.x")
    (expect-lambda-equal true "(λx.x)a" "(λy.y)a")
    (expect-lambda-equal true "λf x.f(f(f x))" "λg y.g(g(g y))")
    (expect-lambda-equal false "λf x.f(f(f x))" "λgy.g(g(g x))")))

(define-test lambda-reduction-test
  :depends-on (parse-lambda-term-test
               lambda-equality-test)
  (macrolet ((is-reduced (expected term)
               `(is term-equal
                    (parse-lambda-term ,expected)
                    (reduce-term (parse-lambda-term ,term)))))
    (is-reduced "x" "x")
    (is-reduced "y" "(λx.x)y")
    (is-reduced "a b" "(λx.λy.x y)a b")
    (is-reduced "λy.λz.a(y z)" "(λx.λy.λz.x(y z))a")
    (is-reduced "c(λa.d a)" "(λa.c a)λa.d a")
    (is-reduced "λy z.z" "(λx.x)λy z.z")
    (is-reduced "y z" "(λx.x y)(λy.y z)")
    (is-reduced "λb.b" "(λa.λb.b)((λx.x x)(λx.x x))")
    (is-reduced "λx.x" "(λz.λy.z)(λx.x)(λa.λb.a)")
    (is-reduced "x" "(λx y z.x y z)(λx.x x)(λx.x)x")
    (is-reduced "λx.x" "(λx.λx.x)(m x x)")
    (is-reduced "λx y.x" "(λn.n(λx.λx y.y)(λx y.x))λf x.x")
    (is-reduced "λx y.y" "(λn.n(λx.λx y.y)(λx y.x))λf x.f x")
    (is-reduced "λx y.y" "(λn.n(λx.λx y.y)(λx y.x))λf x.f(f x)")
    (is-reduced "λx y.y" "(λn.n(λx.λx y.y)(λx y.x))λf x.f(f(f x))")
    (is-reduced "λx y.y" "(λn.n(λx.λx y.y)(λx y.x))λf x.f(f(f(f x)))")
    (is-reduced "λf x.f(f(f x))" "(λn f x.f(n f x))λf x.f(f x)")
    (is-reduced "λf x.f(f(f(f(f(f x)))))" "(λm n f.m(n f))(λf x.f(f(f x)))(λf x.f(f x))")))

(define-test church-numerals-test
  :depends-on (parse-lambda-term-test
               lambda-equality-test)
  (is term-equal (parse-lambda-term "λf x.x") (natural->church 0))
  (is term-equal (parse-lambda-term "λf x.f x") (natural->church 1))
  (is term-equal (parse-lambda-term "λf x.f(f x)") (natural->church 2))
  (is term-equal (parse-lambda-term "λf x.f(f(f x))") (natural->church 3))
  (is term-equal (parse-lambda-term "λf x.f(f(f(f x)))") (natural->church 4))
  (is term-equal (parse-lambda-term "λf x.f(f(f(f(f(f(f x))))))") (natural->church 7))
  (is term-equal (parse-lambda-term "λf x.f(f(f(f(f(f(f(f(f(f(f(f x)))))))))))") (natural->church 12))
  (is = 0 (church->natural (parse-lambda-term "λf x.x")))
  (is = 1 (church->natural (parse-lambda-term "λf x.f x")))
  (is = 3 (church->natural (parse-lambda-term "λf x.f(f(f x))")))
  (is = 8 (church->natural (parse-lambda-term "λf x.f(f(f(f(f(f(f(f x)))))))")))
  (is = 11 (church->natural (parse-lambda-term "λf x.f(f(f(f(f(f(f(f(f(f(f x))))))))))"))))

(define-test barendregt-numerals-test
  :depends-on (parse-combinator-term-test)
  (is term-equal (parse-combinator-term "I") (natural->barendregt 0))
  (is term-equal (parse-combinator-term "V(KI)I") (natural->barendregt 1))
  (is term-equal (parse-combinator-term "V(KI)(V(KI)I)") (natural->barendregt 2))
  (is term-equal (parse-combinator-term "V(KI)(V(KI)(V(KI)I))") (natural->barendregt 3))
  (is term-equal (parse-combinator-term "V(KI)(V(KI)(V(KI)(V(KI)I)))") (natural->barendregt 4))
  (is = 0 (barendregt->natural (parse-combinator-term "I")))
  (is = 1 (barendregt->natural (parse-combinator-term "V(KI)I")))
  (is = 2 (barendregt->natural (parse-combinator-term "V(KI)(V(KI)I)")))
  (is = 3 (barendregt->natural (parse-combinator-term "V(KI)(V(KI)(V(KI)I))")))
  (is = 4 (barendregt->natural (parse-combinator-term "V(KI)(V(KI)(V(KI)(V(KI)I)))"))))

(define-test goedelization-test
  :depends-on (parse-combinator-term-test)
  (macrolet ((is-goedel (n term)
               `(progn
                  (is = ,n (sk->goedel (parse-combinator-term ,term)))
                  (is term-equal (parse-combinator-term ,term) (goedel->sk ,n)))))
    (is-goedel 3312424 "SKK")
    (is-goedel 3333131144321441424 "S(SS)(KS)SK")
    (is-goedel 3333311433214144241432244 "SS(KSS)KS(KK)")
    (is-goedel 3331242433321433124244144 "SKK(KS(SKK)S)")))

(define-test lambda-programs-test
  :depends-on (parse-lambda-term-test
               lambda-equality-test
               lambda-reduction-test
               church-numerals-test)
  (macrolet ((lambda-program-result (path)
               `(run-lambda-program
                 (asdf:system-relative-pathname
                  :ski (merge-pathnames #p"programs/" ,path))
                 (make-broadcast-stream))))
    (is term-equal (natural->church 120) (lambda-program-result #p"fact.lam"))
    (is term-equal (natural->church 40) (lambda-program-result #p"arithm.lam"))
    (is term-equal (natural->church 4) (lambda-program-result #p"pred.lam"))
    (is term-equal (parse-lambda-term "λx y.y") (lambda-program-result #p"basic.lam"))))

(define-test combinator-programs-test
  :depends-on (parse-combinator-term-test
               combinator-reduction-test
               barendregt-numerals-test)
  (macrolet ((combinator-program-result (path)
               `(run-combinator-program
                 (asdf:system-relative-pathname
                  :ski (merge-pathnames #p"programs/" ,path))
                 (make-broadcast-stream))))
    (is term-equal (natural->barendregt 120) (combinator-program-result #p"fact.com"))
    (is term-equal (natural->barendregt 12) (combinator-program-result #p"arithm.com"))
    (is term-equal (get-combinator 'K) (combinator-program-result #p"neg.com"))))
