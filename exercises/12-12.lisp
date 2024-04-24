(defpackage #:ex-12-12
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-12-12)

(defun starling-p (term)
  (let ((x (make-combinator-variable #\x))
        (y (make-combinator-variable #\y))
        (z (make-combinator-variable #\z)))
    (term-equal
     (make-combinator-application
      (make-combinator-application x z)
      (make-combinator-application y z))
     (reduce-term
      (make-combinator-application
       (make-combinator-application
        (make-combinator-application term x)
        y)
       z)))))

(defun solve ()
  (loop with set = (list (get-combinator 'B) (get-combinator 'W**) (get-combinator 'G))
        for n from 1
        for terms = (search-n-terms set n #'starling-p)
        until terms
        finally (report-terms terms)))
