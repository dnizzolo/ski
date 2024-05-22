(asdf:defsystem #:ski
  :description "A simple system to explore combinators."
  :author "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :serial t
  :depends-on (#:esrap)
  :components ((:file "package")
               (:file "base")
               (:file "syntax")
               (:file "ski")
               (:file "lambda")
               (:file "transformations")
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
                             (:file "12-16-07"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:ski/test))))

(asdf:defsystem #:ski/test
  :description "Tests for the ski system."
  :author "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :depends-on (#:ski #:parachute)
  :components ((:module "tests"
                :components ((:file "tests"))))
  :perform (test-op (op c)
                    (uiop:symbol-call :parachute :test :ski/test)))
