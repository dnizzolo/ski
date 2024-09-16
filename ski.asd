(defsystem "ski"
  :description "A simple system to explore combinators."
  :author "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :maintainer "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/dnizzolo/ski"
  :source-control (:git "https://github.com/dnizzolo/ski.git")
  :bug-tracker "https://github.com/dnizzolo/ski/issues"
  :depends-on ("esrap")
  :serial t
  :components ((:file "package")
               (:file "base")
               (:file "combinators")
               (:file "lambda")
               (:file "transformations")
               (:file "syntax")
               (:file "main")
               (:module "exercises"
                :components ((:file "utils")
                             (:file "11-06")
                             (:file "11-10")
                             (:file "11-11")
                             (:file "11-20")
                             (:file "11-23")
                             (:file "11-24")
                             (:file "11-25")
                             (:file "11-26")
                             (:file "11-27")
                             (:file "12-05")
                             (:file "12-12")
                             (:file "12-16")
                             (:file "12-16-01")
                             (:file "12-16-07")))
               (:module "programs"
                :components ((:static-file "aritm.com")
                             (:static-file "aritm.lam")
                             (:static-file "basic.lam")
                             (:static-file "fact.com")
                             (:static-file "fact.lam")
                             (:static-file "neg.com")
                             (:static-file "pred.lam")))
               (:static-file "README.md")
               (:static-file "Makefile")
               (:static-file "LICENSE"))
  :in-order-to ((test-op (test-op "ski/test"))))

(defsystem "ski/test"
  :description "Tests for the ski system."
  :author "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :maintainer "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/dnizzolo/ski"
  :source-control (:git "https://github.com/dnizzolo/ski.git")
  :bug-tracker "https://github.com/dnizzolo/ski/issues"
  :depends-on ("ski" "parachute")
  :components ((:module "tests"
                :components ((:file "tests")
                             (:module "programs"
                              :components ((:static-file "aritm.com")
                                           (:static-file "aritm.lam")
                                           (:static-file "basic.lam")
                                           (:static-file "fact.com")
                                           (:static-file "fact.lam")
                                           (:static-file "neg.com")
                                           (:static-file "pred.lam"))))))
  :perform (test-op (op c) (symbol-call :parachute :test :ski/test)))
