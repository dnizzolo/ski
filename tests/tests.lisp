(defpackage #:ski/test
  (:use #:cl #:ski #:parachute)
  (:shadowing-import-from #:ski #:variable))

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

(define-test combinator->ski-test
  :depends-on (parse-combinator-term-test)
  (macrolet ((is-ski (combinator)
               (let ((collect-variables-form
                       `(loop with g = (make-variable-name-generator)
                              repeat (combinator-arity (get-combinator ,combinator))
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

(define-test lambda-reduction-test
  :depends-on (parse-lambda-term-test)
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
  :depends-on (parse-lambda-term-test)
  (is term-equal (parse-lambda-term "λfx.x") (churchify 0))
  (is term-equal (parse-lambda-term "λfx.fx") (churchify 1))
  (is term-equal (parse-lambda-term "λfx.f(fx)") (churchify 2))
  (is term-equal (parse-lambda-term "λfx.f(f(fx))") (churchify 3))
  (is term-equal (parse-lambda-term "λfx.f(f(f(fx)))") (churchify 4))
  (is term-equal (parse-lambda-term "λfx.f(f(f(f(f(f(fx))))))") (churchify 7))
  (is term-equal (parse-lambda-term "λfx.f(f(f(f(f(f(f(f(f(f(f(fx)))))))))))") (churchify 12))
  (is = 0 (dechurchify (parse-lambda-term "λfx.x")))
  (is = 1 (dechurchify (parse-lambda-term "λfx.fx")))
  (is = 3 (dechurchify (parse-lambda-term "λfx.f(f(fx))")))
  (is = 8 (dechurchify (parse-lambda-term "λfx.f(f(f(f(f(f(f(fx)))))))")))
  (is = 11 (dechurchify (parse-lambda-term "λfx.f(f(f(f(f(f(f(f(f(f(fx))))))))))"))))
