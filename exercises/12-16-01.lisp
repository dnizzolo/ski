(defpackage #:ex-12-16-01
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-12-16-01)

(defun good-p (term)
  (let ((x (make-combinator-variable #\x))
        (y (make-combinator-variable #\y))
        (z (make-combinator-variable #\z))
        (v (make-combinator-variable #\v))
        (w (make-combinator-variable #\w)))
    (term-equal
     (reduce-term
      (reduce #'make-combinator-application
              (list x y z v w)
              :initial-value (get-combinator 'G1)))
     (reduce-term
      (reduce #'make-combinator-application
              (list x y z v w)
              :initial-value term)))))

(defun solve ()
  (loop with set = (list (get-combinator 'B) (get-combinator 'G))
        for n from 1
        for terms = (search-n-terms set n #'good-p)
        until terms
        finally (report-terms terms)))
