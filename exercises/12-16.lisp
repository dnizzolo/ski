(defpackage #:ex-12-16
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-12-16)

(defun mockingbird-p (term)
  (let ((x (make-combinator-variable #\x)))
    (term-equal
     (make-combinator-application x x)
     (reduce-term (make-combinator-application term x)))))

(defun solve ()
  (loop with set = (list (get-combinator 'S) (get-combinator 'T))
        for n from 1
        for terms = (search-n-terms set n #'mockingbird-p)
        until terms
        finally (report-terms terms)))
