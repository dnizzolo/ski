(defpackage #:ex-12-05
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-12-05)

(defun converse-warbler-p (term)
  (let ((x (make-combinator-variable #\x))
        (y (make-combinator-variable #\y)))
    (term-equal
     (make-combinator-application (make-combinator-application y x) x)
     (reduce-term
      (make-combinator-application (make-combinator-application term x) y)))))

(defun solve ()
  (loop with set = (list (get-combinator 'R) (get-combinator 'M2))
        for n from 1
        for terms = (search-n-terms set n #'converse-warbler-p)
        until terms
        finally (report-terms terms)))
