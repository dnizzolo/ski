(defpackage #:ex-11-20
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-11-20)

(defun solve ()
  (loop with robin-p = (make-extensional-equality-predicate (get-combinator 'R))
        with allowed = (list (get-combinator 'B) (get-combinator 'T))
        for n from 1
        for terms = (search-n-terms allowed n robin-p)
        until terms
        finally (report-terms terms)))
