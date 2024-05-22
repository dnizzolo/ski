(defpackage #:ex-12-16-07
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-12-16-07)

(defun makes-starling-p (candidate)
  (let ((x (make-combinator-variable #\x))
        (y (make-combinator-variable #\y))
        (z (make-combinator-variable #\z)))
    (term-equal
     (make-combinator-application
      (make-combinator-application x z)
      (make-combinator-application y z))
     (reduce-term
      (reduce #'make-combinator-application
              (list x y z)
              :initial-value (make-combinator-application
                              (make-combinator-application
                               (get-combinator 'C)
                               candidate)
                              (get-combinator 'W)))))))

(defun solve ()
  (let ((solutions (remove-if-not
                    #'makes-starling-p
                    (compute-full-binary-trees 6 (get-combinator 'Q)))))
    (report-terms solutions)))
