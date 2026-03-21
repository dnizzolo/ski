(defpackage #:ex-12-16-01
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-12-16-01)

(defun solve ()
  (loop with good-p = (make-extensional-equality-predicate (get-combinator 'G1))
        with allowed = (list (get-combinator 'B) (get-combinator 'G))
        for n from 1
        for terms = (search-n-terms allowed n good-p)
        until terms
        finally (report-terms terms)))
