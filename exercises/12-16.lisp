(defpackage #:ex-12-16
  (:use #:cl #:ski #:ex-utils)
  (:shadowing-import-from #:ski #:variable))

(in-package #:ex-12-16)

(defun solve ()
  (loop with mockingbird-p = (make-extensional-equality-predicate (get-combinator 'M))
        with allowed = (list (get-combinator 'S) (get-combinator 'T))
        for n from 1
        for terms = (search-n-terms allowed n mockingbird-p)
        until terms
        finally (report-terms terms)))
