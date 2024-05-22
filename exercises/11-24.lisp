(defpackage #:ex-11-24
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-24)

(defun finch-p (term)
  (let ((x (make-combinator-variable #\x))
        (y (make-combinator-variable #\y))
        (z (make-combinator-variable #\z)))
    (term-equal
     (make-combinator-application (make-combinator-application z y) x)
     (reduce-term
      (reduce #'make-combinator-application
              (list x y z)
              :initial-value term)))))

(defun solve ()
  (loop with set = (list (get-combinator 'B) (get-combinator 'C))
        for n from 1
        for terms = (search-n-terms set n #'finch-p)
        until terms
        finally (report-terms terms)))
