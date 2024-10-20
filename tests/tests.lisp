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
      (parse-combinator-term "xy(zwy)(xz)"))
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
      (parse-lambda-term "xy"))
  (is term-equal
      (make-lambda-application
       (make-lambda-application
        (make-lambda-variable #\v)
        (make-lambda-application
         (make-lambda-variable #\w)
         (make-lambda-variable #\w)))
       (make-lambda-variable #\u))
      (parse-lambda-term "v(ww)u"))
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
      (parse-lambda-term "(λx.x)(λfy.fyy)m")))

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
    (is-reduced "a(bc)" "Babc")
    (is-reduced "a(bc)" "S(KS)Kabc")
    (is-reduced "a(bb)" "S(Ka)(SII)b")
    (is-reduced "a" "VabK")
    (is-reduced "b" "Vab(KI)")
    (is-reduced "xx" "Mx")
    (is-reduced "xx" "STTx")
    (is-reduced "xII" "I2x")
    (is-reduced "xz(yz)" "C(Q(QQ(QQ))Q)Wxyz")
    (is-reduced "xz(yz)" "C(QQ(Q(QQ)Q))Wxyz")))

(define-test lambda->ski-test
  :depends-on (parse-combinator-term-test parse-lambda-term-test)
  (is term-equal
      (parse-combinator-term "S(K(SI))K")
      (lambda->ski (parse-lambda-term "λxy.yx")))
  (is term-equal
      (make-combinator-application
       (make-combinator-variable #\b)
       (make-combinator-variable #\a))
      (reduce-term (lambda->ski (parse-lambda-term "(λxy.yx)ab"))))
  (is term-equal
      (get-combinator 'K)
      (reduce-term (lambda->ski (parse-lambda-term "(λn.n(λx.λxy.y)(λxy.x))λfx.x"))))
  (is term-equal
      (make-combinator-application
       (get-combinator 'K)
       (get-combinator 'I))
      (reduce-term (lambda->ski (parse-lambda-term "(λn.n(λx.λxy.y)(λxy.x))λfx.fx")))))

(define-test lambda->sk-test
  :depends-on (parse-combinator-term-test parse-lambda-term-test)
  (is term-equal
      (parse-combinator-term "S(K(S(SKK)))K")
      (lambda->sk (parse-lambda-term "λxy.yx")))
  (is term-equal
      (make-combinator-application
       (make-combinator-variable #\b)
       (make-combinator-variable #\a))
      (reduce-term (lambda->sk (parse-lambda-term "(λxy.yx)ab"))))
  (is term-equal
      (get-combinator 'K)
      (reduce-term (lambda->sk (parse-lambda-term "(λn.n(λx.λxy.y)(λxy.x))λfx.x"))))
  (is term-equal
      (parse-combinator-term "K(SKK)")
      (reduce-term (lambda->sk (parse-lambda-term "(λn.n(λx.λxy.y)(λxy.x))λfx.fx")))))

(define-test combinator->lambda-test
  :depends-on (combinator-reduction-test parse-lambda-term-test lambda-equality-test)
  (is term-equal
      (combinator->lambda (get-combinator 'M))
      (parse-lambda-term "λx.xx"))
  (is term-equal
      (combinator->lambda (get-combinator 'B))
      (parse-lambda-term "λxyz.x(yz)"))
  (is term-equal
      (combinator->lambda (get-combinator 'L))
      (parse-lambda-term "λxy.x(yy)"))
  (is term-equal
      (combinator->lambda (get-combinator 'W))
      (parse-lambda-term "λxy.xyy"))
  (is term-equal
      (combinator->lambda (get-combinator 'K))
      (parse-lambda-term "λxy.x"))
  (is term-equal
      (combinator->lambda (get-combinator 'I))
      (parse-lambda-term "λx.x"))
  (is term-equal
      (combinator->lambda (get-combinator 'S))
      (parse-lambda-term "λxyz.xz(yz)")))

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
    (expect-lambda-equal true "λfx.f(f(fx))" "λgy.g(g(gy))")
    (expect-lambda-equal false "λfx.f(f(fx))" "λgy.g(g(gx))")))

(define-test lambda-reduction-test
  :depends-on (parse-lambda-term-test
               lambda-equality-test)
  (macrolet ((is-reduced (expected term)
               `(is term-equal
                    (parse-lambda-term ,expected)
                    (reduce-term (parse-lambda-term ,term)))))
    (is-reduced "x" "x")
    (is-reduced "y" "(λx.x)y")
    (is-reduced "ab" "(λx.λy.xy)ab")
    (is-reduced "λy.λz.a(yz)" "(λx.λy.λz.x(yz))a")
    (is-reduced "c(λa.da)" "(λa.ca)λa.da")
    (is-reduced "λyz.z" "(λx.x)λyz.z")
    (is-reduced "yz" "(λx.xy)(λy.yz)")
    (is-reduced "λb.b" "(λa.λb.b)((λx.xx)(λx.xx))")
    (is-reduced "λx.x" "(λz.λy.z)(λx.x)(λa.λb.a)")
    (is-reduced "x" "(λxyz.xyz)(λx.xx)(λx.x)x")
    (is-reduced "λx.x" "(λx.λx.x)(mxx)")
    (is-reduced "λxy.x" "(λn.n(λx.λxy.y)(λxy.x))λfx.x")
    (is-reduced "λxy.y" "(λn.n(λx.λxy.y)(λxy.x))λfx.fx")
    (is-reduced "λxy.y" "(λn.n(λx.λxy.y)(λxy.x))λfx.f(fx)")
    (is-reduced "λxy.y" "(λn.n(λx.λxy.y)(λxy.x))λfx.f(f(fx))")
    (is-reduced "λxy.y" "(λn.n(λx.λxy.y)(λxy.x))λfx.f(f(f(fx)))")
    (is-reduced "λfx.f(f(fx))" "(λnfx.f(nfx))λfx.f(fx)")
    (is-reduced "λfx.f(f(f(f(f(fx)))))" "(λmnf.m(nf))(λfx.f(f(fx)))(λfx.f(fx))")))

(define-test church-numerals-test
  :depends-on (parse-lambda-term-test
               lambda-equality-test)
  (is term-equal (parse-lambda-term "λfx.x") (natural->church 0))
  (is term-equal (parse-lambda-term "λfx.fx") (natural->church 1))
  (is term-equal (parse-lambda-term "λfx.f(fx)") (natural->church 2))
  (is term-equal (parse-lambda-term "λfx.f(f(fx))") (natural->church 3))
  (is term-equal (parse-lambda-term "λfx.f(f(f(fx)))") (natural->church 4))
  (is term-equal (parse-lambda-term "λfx.f(f(f(f(f(f(fx))))))") (natural->church 7))
  (is term-equal (parse-lambda-term "λfx.f(f(f(f(f(f(f(f(f(f(f(fx)))))))))))") (natural->church 12))
  (is = 0 (church->natural (parse-lambda-term "λfx.x")))
  (is = 1 (church->natural (parse-lambda-term "λfx.fx")))
  (is = 3 (church->natural (parse-lambda-term "λfx.f(f(fx))")))
  (is = 8 (church->natural (parse-lambda-term "λfx.f(f(f(f(f(f(f(fx)))))))")))
  (is = 11 (church->natural (parse-lambda-term "λfx.f(f(f(f(f(f(f(f(f(f(fx))))))))))"))))

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
                  :ski (merge-pathnames #p"tests/programs/" ,path))
                 (make-broadcast-stream))))
    (is term-equal (natural->church 120) (lambda-program-result #p"fact.lam"))
    (is term-equal (natural->church 40) (lambda-program-result #p"aritm.lam"))
    (is term-equal (natural->church 4) (lambda-program-result #p"pred.lam"))
    (is term-equal (parse-lambda-term "λxy.y") (lambda-program-result #p"basic.lam"))))

(define-test combinator-programs-test
  :depends-on (parse-combinator-term-test
               combinator-reduction-test
               barendregt-numerals-test)
  (macrolet ((combinator-program-result (path)
               `(run-combinator-program
                 (asdf:system-relative-pathname
                  :ski (merge-pathnames #p"tests/programs/" ,path))
                 (make-broadcast-stream))))
    (is term-equal (natural->barendregt 120) (combinator-program-result #p"fact.com"))
    (is term-equal (natural->barendregt 12) (combinator-program-result #p"aritm.com"))
    (is term-equal (get-combinator 'K) (combinator-program-result #p"neg.com"))))
